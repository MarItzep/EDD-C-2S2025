<template>
  <div class="box">
    <h2>Iniciar sesión</h2>
    <form @submit.prevent="login">
      <input v-model="credentials.username" placeholder="Usuario" required />
      <input v-model="credentials.password" type="password" placeholder="Contraseña" required />
      <button type="submit">Entrar</button>
    </form>

    <p v-if="message">{{ message }}</p>
  </div>
</template>

<script>
import axios from 'axios';

export default {
  emits: ['logged-in'],
  data() {
    return {
      credentials: { username: '', password: '' },
      message: '',
    };
  },
  methods: {
    async login() {
      this.message = '';
      try {
        const res = await axios.post('/login', this.credentials);
        this.message = res.data.message;
        this.$emit('logged-in', res.data.token);
      } catch (err) {
        this.message = err.response?.data?.error || 'Error de login';
      }
    },
  },
};
</script>

<style scoped>
.box {
  border: 1px solid #ddd;
  padding: 15px;
  border-radius: 8px;
  flex: 1;
}
input, button {
  display: block;
  width: 100%;
  margin-top: 5px;
  padding: 8px;
}
</style>
