<template>
  <div class="box">
    <h2>Registro</h2>
    <form @submit.prevent="register">
      <input v-model="user.username" placeholder="Usuario" required />
      <input v-model="user.password" type="password" placeholder="Contraseña" required />
      <input v-model="user.fullName" placeholder="Nombre completo" />
      <input v-model="user.email" type="email" placeholder="Email" />
      <textarea v-model="user.bio" placeholder="Bio o descripción"></textarea>

      <h3>Mascotas</h3>
      <div v-for="(p, i) in pets" :key="i" class="pet">
        <input v-model="p.name" placeholder="Nombre" />
        <input v-model="p.species" placeholder="Especie" />
        <input v-model.number="p.age" type="number" placeholder="Edad" />
        <button type="button" @click="removePet(i)">🗑️</button>
      </div>

      <button type="button" @click="addPet">➕ Agregar mascota</button>
      <br />
      <button type="submit">Registrar</button>
    </form>

    <p v-if="message">{{ message }}</p>
  </div>
</template>

<script>
import axios from 'axios';

export default {
  emits: ['user-registered'],
  data() {
    return {
      user: { username: '', password: '', fullName: '', email: '', bio: '' },
      pets: [],
      message: '',
    };
  },
  methods: {
    addPet() {
      this.pets.push({ name: '', species: '', age: 0 });
    },
    removePet(index) {
      this.pets.splice(index, 1);
    },
    async register() {
      this.message = '';
      try {
        const payload = {
          username: this.user.username,
          password: this.user.password,
          fullName: this.user.fullName,
          email: this.user.email,
          profile: { bio: this.user.bio },
          pets: this.pets,
        };
        const res = await axios.post('/register', payload);
        this.message = res.data.message;
        this.$emit('user-registered', this.message);
      } catch (err) {
        this.message = err.response?.data?.error || 'Error de registro';
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
.pet {
  display: flex;
  gap: 8px;
  align-items: center;
  margin-bottom: 5px;
}
input, textarea, button {
  display: block;
  width: 100%;
  margin-top: 5px;
  padding: 8px;
}
button {
  cursor: pointer;
}
</style>
