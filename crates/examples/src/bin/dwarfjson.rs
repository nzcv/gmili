use std::{borrow::Cow, collections::HashMap, fs::File, vec};
use gimli::{DebugStrOffset, RunTimeEndian};
use memmap2::Mmap;
use object::{Object, ObjectSection};

// Abbreviations for some lengthy gimli types
type R<'a> = gimli::EndianSlice<'a, RunTimeEndian>;
type DIE<'a> = gimli::DebuggingInformationEntry<'a,'a,R<'a>,usize>;
type CU<'a> = gimli::Unit<R<'a>, usize>;
type GimliDwarf<'a> = gimli::Dwarf<R<'a>>;

/// Represents a location of some type/tag in the DWARF information
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub header: gimli::DebugInfoOffset,
    pub offset: gimli::UnitOffset,
}

/// Represents a struct type
#[derive(Clone, Copy, Debug)]
pub struct Struct {
    pub location: Location,
}

impl Tagged for Struct {
    fn new(location: Location) -> Self {
        Self { location }
    }
    fn tag() -> gimli::DwTag {
        gimli::DW_TAG_class_type
    }
}

/// Error type for parsing/loading DWARF information
#[derive(thiserror::Error, Debug)]
pub enum Error {
    // Fatal
    #[error("failed to load dwarf info from file")]
    DwarfLoadError(String),

    #[error("object failed to parse file")]
    ObjectError(#[from] object::Error),

    #[error("object failed to parse file")]
    GimliError(#[from] gimli::Error),

    #[error("any error")]
    AnyError(String),

    #[error("failed when attempting to get offset of a UnitHeader")]
    HeaderOffsetError,

    #[error("failed when attempting to get some CU")]
    CUError(String),

    #[error("failed when attempting to get some DIE")]
    DIEError(String),

    #[error("failed due to unimplemented functionality")]
    UnimplementedError(String),

    // Non-Fatal
    #[error("failure when attempting to find a Name Attribute")]
    NameAttributeNotFound,

    #[error("failure when attempting to find a Type Attribute")]
    TypeAttributeNotFound,

    #[error("failure when attempting to find a ByteSize Attribute")]
    ByteSizeAttributeNotFound,

    #[error("failure when attempting to find a BitSize Attribute")]
    BitSizeAttributeNotFound,

    #[error("failure when attempting to find a MemberLocation Attribute")]
    MemberLocationAttributeNotFound,

    #[error("failure when attempting to find an Alignment Attribute")]
    AlignmentAttributeNotFound,
}

/// Represents DWARF data
pub struct Dwarf<'a> {
    dwarf_cow: gimli::Dwarf<Cow<'a, [u8]>>,
    endianness: RunTimeEndian
}

impl<'a> Dwarf<'a> {
    pub fn load(data: impl object::ReadRef<'a>) -> Result<Self, Error> {
        let object = object::File::parse(data)?;

        let endianness = if object.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        let load_section = |id: gimli::SectionId|
        -> Result<Cow<[u8]>, gimli::Error> {
            println!("Loading section: {}", id.name());
            match object.section_by_name(id.name()) {
                Some(ref section) => Ok(section
                    .uncompressed_data()
                    .unwrap_or(Cow::Borrowed(&[][..]))),
                None => Ok(Cow::Borrowed(&[][..])),
            }
        };

        // Load all of the sections
        let dwarf_cow = gimli::Dwarf::load(&load_section).unwrap();

        Ok(Self{dwarf_cow, endianness})
    }
}


pub trait BorrowableDwarf {
    fn borrow_dwarf<F, R>(&self, f: F) -> R
    where F: FnOnce(&GimliDwarf) -> R;
}

impl BorrowableDwarf for Dwarf<'_> {
    fn borrow_dwarf<F,R>(&self, f: F) -> R
    where F: FnOnce(&GimliDwarf) -> R {
        let borrow_section: &dyn for<'b> Fn(&'b Cow<[u8]>)
        -> gimli::EndianSlice<'b, gimli::RunTimeEndian> =
        &|section| gimli::EndianSlice::new(section, self.endianness);

        let dwarf = self.dwarf_cow.borrow(borrow_section);
        f(&dwarf)
    }
}

pub trait DwarfContext {
    fn entry_context<F,R>(&self, loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&DIE) -> R;

    fn unit_context<F,R>(&self, loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&CU) -> R;
}
impl DwarfContext for Dwarf<'_> {
    fn entry_context<F,R>(&self, loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&DIE) -> R {
        self.unit_context(loc, |unit| -> Result<R, Error> {
            let entry = match unit.entry(loc.offset) {
                Ok(entry) => entry,
                Err(_) => {
                    return Err(
                        Error::DIEError(
                            format!("Failed to find DIE at location: {loc:?}")
                        )
                    );
                }
            };
            Ok(f(&entry))
        })?
    }

    fn unit_context<F,R>(&self, loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&CU) -> R {
        self.borrow_dwarf(|dwarf| {
            let debug_info = dwarf.debug_info;
            let unit_header = match debug_info.header_from_offset(loc.header) {
                Ok(header) => header,
                Err(e) => return Err(
                    Error::CUError(
                        format!("Failed to seek to UnitHeader, error: {}", e)
                    ))
            };
            let unit = gimli::Unit::new(dwarf, unit_header).unwrap();
            Ok(f(&unit))
        })
    }
}

impl DwarfContext for CU<'_> {
    fn entry_context<F,R>(&self, loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&DIE) -> R {
        let entry = match self.entry(loc.offset) {
            Ok(entry) => entry,
            Err(_) => {
                return Err(
                    Error::DIEError(
                        format!("Failed to find DIE at location: {loc:?}")
                    )
                );
            }
        };
        Ok(f(&entry))
    }

    fn unit_context<F,R>(&self, _loc: &Location, f: F) -> Result<R, Error>
    where F: FnOnce(&CU) -> R {
        Ok(f(self))
    }
}
/// A struct to hold the HashMap key for `get_named_structs_map`
#[derive(Eq, Hash, PartialEq)]
pub struct StructHashKey {
    /// The name of the struct
    pub name: String,

    /// The size of the struct in bytes
    pub byte_size: usize,

    /// A tuple of: member name, member offset
    pub members: Vec<(String, usize)>
}

pub trait Tagged {
    fn new(location: Location) -> Self;
    fn tag() -> gimli::DwTag;
}

enum AttrValue {
    String(String),
    DebugInfoOffset(usize),
    TypeInfoOffset(usize),
}

fn get_entry_byte_size(entry: &DIE) -> Option<usize> {
    let mut attrs = entry.attrs();
    while let Ok(Some(attr)) = &attrs.next() {
        if attr.name() == gimli::DW_AT_byte_size {
            return attr.udata_value().map(|v| v as usize)
        }
    }
    None
}

fn for_each_die<T: Tagged, F>(dwarf: &GimliDwarf, mut f: F) -> Result<(), Error> where F: FnMut(&CU, &DIE, Location) -> Result<bool, Error> {
    let mut unit_headers = dwarf.debug_info.units();
    while let Ok(Some(header)) = unit_headers.next() {
        println!("{:?}", header);
        let unit = match dwarf.unit(header) {
            Ok(unit) => unit,
            Err(_) => continue
        };

        let unit_ref = unit.unit_ref(dwarf);
        
        let mut entries = unit.entries();
        'entries:
        while let Ok(Some((_delta_depth, entry))) = entries.next_dfs() {
            if entry.tag() != T::tag() {
                continue;
            }            
            let mut name = String::new();
            let to_string = |e| -> Result<String, Error> {                
                match e {
                    gimli::AttributeValue::DebugStrRef(offset) => {
                        if let Ok(s) = unit_ref.string(offset) {
                            Ok(s.to_string()?.to_string())                            
                        } else {
                            Err(Error::AnyError("failed to get string".to_string()))
                        }
                    }                            
                    _ => {
                        Err(Error::AnyError("failed to get string".to_string()))
                    },
                }
            };
            let mut attrs = entry.attrs();
            while let Ok(Some(attr)) = attrs.next() {
                if attr.name() == gimli::DW_AT_declaration {
                    continue 'entries
                }
                if attr.name() == gimli::DW_AT_name {
                    name = to_string(attr.value())?;
                }
            }
            println!("_delta_depth: {} {} {}", _delta_depth, entry.tag(), name);
            let header_offset =
                match header.offset().as_debug_info_offset() {
                    Some(offset) => offset,
                    // should be unreachable
                    None => return Err(Error::HeaderOffsetError)
            };

            let location = Location {
                header: header_offset,
                offset: entry.offset(),
            };

            // return if function returns true
            if f(&unit, entry, location)? {
                return Ok(())
            }
        }
    }
    Ok(())
}

/// Represents a field of a struct or union
#[derive(Clone, Copy, Debug)]
pub struct Member {
    pub location: Location,
}

fn u_members(loc: &Location,unit: &CU, dwarf: &GimliDwarf) -> Result<Vec<Member>, Error> {
    let mut members: Vec<Member> = Vec::new();
    let mut entries = {
        match unit.entries_at_offset(loc.offset) {
            Ok(entries) => entries,
            _ => return Err(Error::DIEError(
               format!("Failed to seek to DIE at {:?}", loc)
            ))
        }
    };
    if entries.next_dfs().is_err() {
        return Err(Error::DIEError(
            format!("Failed to find next DIE at {:?}", loc)
        ))
    }
    let unit_ref = unit.unit_ref(dwarf);
    while let Ok(Some((_, entry))) = entries.next_dfs() {
        if entry.tag() != gimli::DW_TAG_member {
            break;
        }

        // let mut name = String::new();
        // let to_string = |e| -> Result<String, Error> {                
        //     match e {
        //         gimli::AttributeValue::DebugStrRef(offset) => {
        //             if let Ok(s) = unit_ref.string(offset) {
        //                 Ok(s.to_string()?.to_string())                            
        //             } else {
        //                 Err(Error::AnyError("failed to get string".to_string()))
        //             }
        //         }                            
        //         _ => {
        //             Err(Error::AnyError("failed to get string".to_string()))
        //         },
        //     }
        // };
        // let mut attrs = entry.attrs();
        // while let Ok(Some(attr)) = attrs.next() {            
        //     if attr.name() == gimli::DW_AT_name {
        //         name = to_string(attr.value())?;
        //     }
        // }
        // println!("member {}", name);
        
        let location = Location {
            header: loc.header,
            offset: entry.offset(),
        };
        members.push(Member { location });
    };
    Ok(members)
}

// Try to retrieve a string from the debug_str section for a given offset
fn from_dbg_str_ref(dwarf: &GimliDwarf, str_ref: DebugStrOffset<usize>) -> Option<String> {
    if let Ok(str_ref) = dwarf.debug_str.get_str(str_ref) {
        let str_ref = str_ref.to_string_lossy();
        return Some(str_ref.to_string());
    }
    None
}

fn get_entry_name(dwarf: &GimliDwarf, entry: &DIE) -> Option<String> {
    let mut attrs = entry.attrs();
    while let Ok(Some(attr)) = &attrs.next() {
        if attr.name() == gimli::DW_AT_name {
            match attr.value() {
                gimli::AttributeValue::String(str) => {
                    if let Ok(str) = str.to_string() {
                        return Some(str.to_string())
                    }
                }
                gimli::AttributeValue::DebugStrRef(strref) => {
                    return from_dbg_str_ref(dwarf, strref)
                }
                _ => { }
            };
        }
    }
    None
}

fn u_name(dwarf: &GimliDwarf, unit: &CU, loc: &Location) -> Result<String, Error> {
    if let Some(name) = unit.entry_context(loc, |entry| {
        get_entry_name(dwarf, entry)
    })? {
        Ok(name)
    } else {
        Err(Error::NameAttributeNotFound)
    }
}

pub trait DwarfLookups : BorrowableDwarf
where Self: Sized + DwarfContext {
    fn get_fg_named_structs_map(&self) -> Result<HashMap<StructHashKey, Struct>, Error> {
        let mut struct_locations: HashMap<StructHashKey, Struct> = {
            HashMap::new()
        };
        self.borrow_dwarf(|dwarf| {
            let _ = for_each_die::<Struct, _>(dwarf, |unit, entry, loc| {
                let name = get_entry_name(dwarf, &entry).ok_or(Error::NameAttributeNotFound)?;
                let members = u_members(&loc, unit, dwarf)?;
                let byte_size = get_entry_byte_size(&entry).ok_or(Error::ByteSizeAttributeNotFound)?;
                println!("{} members {}", name, members.len());
                let members: Vec<(String,usize)> = {
                    members.iter().map(|m| -> Result<(String,usize), Error> {
                        Ok((u_name(dwarf, unit, &m.location).unwrap_or("".to_string()), 0))
                    }).collect::<Result<Vec<_>, _>>()?
                };
                let key = StructHashKey {
                    name: name,
                    byte_size: byte_size,
                    members: members
                };
                struct_locations.insert(key, Struct { location: loc });
                Ok(false)
            });
        });
        Ok(struct_locations)   
    }
}
impl DwarfLookups for Dwarf<'_> {}

fn main() -> anyhow::Result<()> {    
    let path = std::env::args().nth(1).ok_or_else(|| anyhow::anyhow!("No file path provided"))?;
    println!("Path: {}", path);
    let file = File::open(&path)?;
    let mmap = unsafe { Mmap::map(&file) }?;
    let dwarf = Dwarf::load(&*mmap)?;
    let struct_map = dwarf.get_fg_named_structs_map()?;
    // for (key, struc) in struct_map.into_iter() {
        // let members = struc.members(&dwarf)?.len();
    //     println!("{}\t{}", key.name, members);
    // };

    Ok(())
}
