export const RAIL_COLORS:Record<string,string>={
  all_rail:'#ed5948',
  metro:'#3d83b5',
  metro_cercanias:'#8b6aa8',
  cercanias:'#d64b4b',
  metro_ligero:'#d0a13b',
  tranvia_parla:'#4e9d71',
  mlt:'#d0a13b'
};

export const ENTITY_DASHES=['','3 3','9 4','12 3 3 3'];
export const POINT_DASHES:Record<string,string>={entrance:'',platform:'3 3'};

export function primaryMode(modes:string){return modes.split('|')[0]||'all_rail';}
