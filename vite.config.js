import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  server: {
    port: 3000,
    proxy: {
      '/guess': {
        target: 'http://localhost:5173',
        changeOrigin: true
      },
      '/reset': {
        target: 'http://localhost:5173',
        changeOrigin: true
      }
    }
  }
})