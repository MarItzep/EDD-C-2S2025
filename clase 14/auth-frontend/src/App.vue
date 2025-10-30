<template>
  <div class="container">
    <h1>🐾 Sistema de Autenticación con Vue + Node</h1>

    <div class="grid">
      <RegisterForm @user-registered="userRegistered" />
      <LoginForm @logged-in="loggedIn" />
    </div>

    <ProfileView v-if="token" :token="token" />
  </div>
</template>

<script>
import RegisterForm from './components/RegisterForm.vue';
import LoginForm from './components/LoginForm.vue';
import ProfileView from './components/ProfileView.vue';

export default {
  components: { RegisterForm, LoginForm, ProfileView },
  data() {
    return {
      token: localStorage.getItem('token') || null,
    };
  },
  methods: {
    userRegistered(msg) {
      alert('✅ ' + msg);
    },
    loggedIn(token) {
      this.token = token;
      localStorage.setItem('token', token);
    },
  },
};
</script>

<style>
body {
  font-family: 'Arial', sans-serif;
  background: #f6f6f6;
  margin: 0;
  padding: 0;
}
.container {
  max-width: 900px;
  margin: 30px auto;
  background: #fff;
  padding: 20px;
  border-radius: 12px;
  box-shadow: 0 0 10px #ddd;
}
.grid {
  display: flex;
  justify-content: space-between;
  gap: 30px;
}
h1 {
  text-align: center;
  margin-bottom: 30px;
}
</style>
