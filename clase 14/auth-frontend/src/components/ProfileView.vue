<template>
  <div class="box">
    <h2>Perfil del Usuario</h2>

    <div v-if="!profile">
      <input v-model="query" placeholder="Usuario a buscar" />
      <button @click="fetchProfile">Ver perfil</button>
    </div>

    <div v-if="profile">
      <h3>{{ profile.fullName }} ({{ profile.username }})</h3>
      <p><b>Email:</b> {{ profile.email }}</p>
      <p><b>Bio:</b> {{ profile.profile?.bio || 'Sin descripción' }}</p>

      <h4>Mascotas</h4>
      <ul>
        <li v-for="(pet, i) in profile.pets" :key="i">
          🐶 {{ pet.name }} — {{ pet.species }} — {{ pet.age }} años
        </li>
      </ul>

      <button @click="profile = null">🔙 Volver</button>
    </div>

    <p v-if="error" style="color: red">{{ error }}</p>
  </div>
</template>

<script>
import axios from 'axios';

export default {
  props: ['token'],
  data() {
    return {
      query: '',
      profile: null,
      error: '',
    };
  },
  methods: {
    async fetchProfile() {
      this.error = '';
      try {
        const res = await axios.get(`/profile/${this.query}`, {
          headers: { Authorization: `Bearer ${this.token}` },
        });
        this.profile = res.data.user;
      } catch (err) {
        this.error = err.response?.data?.error || 'Error al obtener perfil';
      }
    },
  },
};
</script>

<style scoped>
.box {
  margin-top: 20px;
  padding: 15px;
  border: 1px solid #ddd;
  border-radius: 8px;
}
input, button {
  display: block;
  margin-top: 8px;
  padding: 8px;
}
</style>
