import { defineConfig } from 'vite';
import { readFileSync } from 'node:fs';

function maplibreWorkerAssets(){
  const distribution=new URL('./node_modules/maplibre-gl/dist/',import.meta.url);
  return {
    name:'maplibre-worker-assets',
    generateBundle(this:{emitFile:(asset:{type:'asset';fileName:string;source:Buffer})=>void}){
      for(const name of ['maplibre-gl-worker.mjs','maplibre-gl-shared.mjs']){
        this.emitFile({type:'asset',fileName:`assets/${name}`,source:readFileSync(new URL(name,distribution))});
      }
    }
  };
}

export default defineConfig({
  base: './',
  plugins:[maplibreWorkerAssets()],
  build: {
    outDir: '../site_phase7',
    emptyOutDir: true,
    sourcemap: true,
    assetsInlineLimit: 0
  }
});
