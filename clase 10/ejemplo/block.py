import json
import hashlib
import time

# clase  usuario 
class Usuario:
    def __init__(self, nombres, apellidos, correo, edad, contrasenia):
        self.Nombres = nombres
        self.Apellidos = apellidos
        self.Correo = correo
        self.Edad = edad
        self.Contrasenia = contrasenia

# clase bloque
# inicializa el bloque con los datos del usuario y calcula el hash del bloque
class Block:
    def __init__(self, index, data, previous_hash=""):
        self.Index = index
        self.Timestamp = time.time()
        self.Data = data
        self.PreviousHash = previous_hash
        self.Nonce = 0
        self.Hash = self.calculate_hash()
    # calcula el hash del bloque
    def calculate_hash(self):
        block_string = f"{self.Index}{self.Timestamp}{self.Data.__dict__}{self.Nonce}{self.PreviousHash}"
        return hashlib.sha256(block_string.encode()).hexdigest()

# clase blockchain
# maneja la cadena de bloques, añade nuevos bloques y genera una representación visual en formato DOT
class Blockchain:
    def __init__(self):
        self.Chain = [self.create_genesis_block()]
    # crea el bloque génesis
    # Que es el genesis block
    # El bloque génesis es el primer bloque de una cadena de bloques 
    # y sirve como punto de partida para la cadena.
    def create_genesis_block(self):
        return Block(0, Usuario("Genesis", "", "genesis@block.com", 0, ""), "0")
    # obtiene el último bloque de la cadena
    def get_last_block(self):
        return self.Chain[-1]

    def add_block(self, data):
        last_block = self.get_last_block()
        new_block = Block(len(self.Chain), data, last_block.Hash)
        self.Chain.append(new_block)

    def generar_dot(self, filepath):
        with open(filepath, "w", encoding="utf-8") as f:
            f.write("digraph Blockchain {\n")
            f.write("  node [shape=box, style=filled, color=lightblue];\n")

            for i, block in enumerate(self.Chain):
                user = block.Data
                label = (
                    f"Index: {block.Index}\\n"
                    f"Timestamp: {block.Timestamp}\\n"
                    f"Usuario: {user.Nombres} {user.Apellidos}\\n"
                    f"Correo: {user.Correo}\\n"
                    f"Edad: {user.Edad}\\n"
                    f"Contraseña: {user.Contrasenia}\\n"
                    f"Nonce: {block.Nonce}\\n"
                    f"Hash: {block.Hash[:15]}...\\n"
                    f"PrevHash: {block.PreviousHash[:15]}..."
                )
                f.write(f'  Block{i} [label="{label}"];\n')

                if i > 0:
                    f.write(f'  Block{i-1} -> Block{i};\n')

            f.write("}\n")


# función para encriptar la contraseña usando SHA-256
def encrypt_sha256(text):
    return hashlib.sha256(text.encode()).hexdigest()


def main():
    # Leer archivo JSON
    path = "usuarios.json"
    with open(path, "r", encoding="utf-8") as f:
        usuarios_data = json.load(f)
    # Crear objetos Usuario
    usuarios = []
    for user in usuarios_data:
        contrasenia_hash = encrypt_sha256(user["Contrasenia"])
        usuarios.append(Usuario(user["Nombres"], user["Apellidos"], user["Correo"], user["Edad"], contrasenia_hash))

    # Crear blockchain y añadir usuarios
    blockchain = Blockchain()
    for user in usuarios:
        blockchain.add_block(user)

    # Mostrar blockchain en consola
    for block in blockchain.Chain:
        print("--------- BLOQUE ---------")
        print(f"Index: {block.Index}")
        print(f"Timestamp: {block.Timestamp}")
        print(f"Usuario: {block.Data.Nombres} {block.Data.Apellidos}")
        print(f"Correo: {block.Data.Correo}")
        print(f"Edad: {block.Data.Edad}")
        print(f"Contraseña (hash): {block.Data.Contrasenia}")
        print(f"Nonce: {block.Nonce}")
        print(f"Previous Hash: {block.PreviousHash}")
        print(f"Hash: {block.Hash}")
        print()

    # Guardar blockchain en JSON
    blockchain_data = [
        {
            "Index": block.Index,
            "Timestamp": block.Timestamp,
            "Usuario": {
                "Nombres": block.Data.Nombres,
                "Apellidos": block.Data.Apellidos,
                "Correo": block.Data.Correo,
                "Edad": block.Data.Edad,
                "Contrasenia": block.Data.Contrasenia,
            },
            "Nonce": block.Nonce,
            "PreviousHash": block.PreviousHash,
            "Hash": block.Hash,
        }
        for block in blockchain.Chain
    ]
    # Guardar en archivo blockchain.json
    blockchain_file = "blockchain.json"
    with open(blockchain_file, "w", encoding="utf-8") as f:
        json.dump(blockchain_data, f, indent=4)

    print(f"Blockchain guardada en: {blockchain_file}")

    # Generar archivo DOT
    dot_file = "blockchain.dot"
    blockchain.generar_dot(dot_file)
    print(f"Archivo DOT generado: {dot_file}")

# Ejecutar la función principal
if __name__ == "__main__":
    main()
