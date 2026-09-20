import {describe,expect,it} from 'vitest';
import {stationName} from './labels';

describe('stationName',()=>{
  it('restores Spanish accents and official display forms',()=>{
    expect(stationName('Avenida de America')).toBe('Avenida de América');
    expect(stationName('Anton Martin')).toBe('Antón Martín');
    expect(stationName('Pinar de Chamartin')).toBe('Pinar de Chamartín');
    expect(stationName('Plaza Eliptica')).toBe('Plaza Elíptica');
    expect(stationName('Ronda de la Comunicacion')).toBe('Ronda de la Comunicación');
    expect(stationName('Alcobendas-s.s. de los Reyes')).toBe('Alcobendas–San Sebastián de los Reyes');
    expect(stationName('Barrial-c.com.pozuelo, el')).toBe('El Barrial–Centro Comercial Pozuelo');
    expect(stationName('San Fermin-orcasur')).toBe('San Fermín–Orcasur');
    expect(stationName('Águilas, las')).toBe('Las Águilas');
  });
});
