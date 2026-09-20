import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir:'./tests',
  timeout:90_000,
  expect:{timeout:15_000},
  reporter:'line',
  outputDir:'../validation/phase7/playwright-results',
  use:{baseURL:'http://127.0.0.1:4177',channel:'chrome',trace:'retain-on-failure'},
  webServer:{command:'npm run preview -- --port 4177',url:'http://127.0.0.1:4177',reuseExistingServer:false,timeout:120_000},
  projects:[
    {name:'desktop',use:{...devices['Desktop Chrome'],viewport:{width:1440,height:1000}}},
    {name:'mobile',use:{browserName:'chromium',channel:'chrome',viewport:{width:390,height:844},deviceScaleFactor:1,isMobile:true,hasTouch:true}}
  ]
});
