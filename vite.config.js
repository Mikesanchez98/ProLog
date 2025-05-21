import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      '/guess': {
        target: 'http://localhost:8080',
        changeOrigin: true
      },
      '/reset': {
        target: 'http://localhost:8080',
        changeOrigin: true
      }
    }
  }
})