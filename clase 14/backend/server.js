// server.js
const express = require('express');
const bodyParser = require('body-parser');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const cors = require('cors');
const fs = require('fs');
const { SinglyLinkedList } = require('./estructura');

const app = express();
app.use(bodyParser.json());
app.use(cors());

const PORT = process.env.PORT || 3000;
const JWT_SECRET = process.env.JWT_SECRET || 'clave_super_segura';

const usersList = new SinglyLinkedList();


// CARGA AUTOMÁTICA DE DATOS
async function loadSampleData() {
  try {
    const data = JSON.parse(fs.readFileSync('./cargaData.json', 'utf8'));
    for (const user of data.users) {
      const passwordHash = await bcrypt.hash(user.password, 10);
      usersList.insertAtHead({
        username: user.username,
        passwordHash,
        fullName: user.fullName,
        email: user.email,
        profile: user.profile,
        pets: user.pets
      });
    }
    console.log(` ${data.users.length} usuarios cargados automáticamente.`);
  } catch (err) {
    console.error('⚠️ Error cargando cargaData.json:', err.message);
  }
}
loadSampleData();

// Funciones y endpoints del servidor

function findUser(username) {
  return usersList.find(u => u.username === username);
}

app.get('/users', (req, res) => {
  const allUsers = usersList.toArray().map(u => {
    const { passwordHash, ...safe } = u;
    return safe;
  });
  res.json(allUsers);
}
);

app.post('/register', async (req, res) => {
  try {
    const { username, password, fullName, email, profile, pets } = req.body;
    if (!username || !password)
      return res.status(400).json({ error: 'username y password requeridos' });

    if (findUser(username))
      return res.status(409).json({ error: 'Usuario ya existe' });

    const passwordHash = await bcrypt.hash(password, 10);
    const user = { username, passwordHash, fullName, email, profile, pets };
    usersList.insertAtHead(user);
    const { passwordHash: _, ...safe } = user;
    res.status(201).json({ message: 'Registrado', user: safe });
  } catch (err) {
    console.error(err);
    res.status(500).json({ error: 'Error interno' });
  }
});

app.post('/login', async (req, res) => {
  const { username, password } = req.body;
  console.log('Intento de login:', username);
  console.log('Password recibido:', password);
  
  const user = findUser(username);
  if (!user) return res.status(401).json({ error: 'Credenciales inválidas' });

  const ok = await bcrypt.compare(password, user.passwordHash);
  if (!ok) return res.status(401).json({ error: 'Credenciales inválidas' });

  const token = jwt.sign({ username }, JWT_SECRET, { expiresIn: '2h' });
  res.json({ message: 'Autenticado', token });
});

function authMiddleware(req, res, next) {
  const auth = req.headers.authorization;
  if (!auth) return res.status(401).json({ error: 'Token requerido' });
  const parts = auth.split(' ');
  if (parts.length !== 2 || parts[0] !== 'Bearer')
    return res.status(401).json({ error: 'Formato de token inválido' });
  const token = parts[1];
  try {
    req.user = jwt.verify(token, JWT_SECRET);
    next();
  } catch {
    res.status(401).json({ error: 'Token inválido o expirado' });
  }
}

app.get('/profile/:username', authMiddleware, (req, res) => {
  const user = findUser(req.params.username);
  if (!user) return res.status(404).json({ error: 'Usuario no encontrado' });
  const { passwordHash, ...safe } = user;
  res.json({ user: safe });
});

app.get('/_admin/all-users', (req, res) => {
  const all = usersList.toArray().map(u => {
    const { passwordHash, ...safe } = u;
    return safe;
  });
  res.json(all);
});

app.listen(PORT, () => {
  console.log(` Servidor corriendo en http://localhost:${PORT}`);
});
