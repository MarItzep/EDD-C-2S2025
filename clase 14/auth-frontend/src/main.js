import { createApp } from 'vue';
import App from './App.vue';
import axios from 'axios';

axios.defaults.baseURL = 'http://localhost:3000'; //  backend Node.js
axios.defaults.headers.post['Content-Type'] = 'application/json';

const app = createApp(App);
app.config.globalProperties.$axios = axios;
app.mount('#app');
