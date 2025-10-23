
# Manual de uso de Diccionarios y Tipos en Object Pascal (LZW)

##  Introducción
El siguiente documento explica cómo se utilizan los **diccionarios** y otros **tipos de datos personalizados** dentro del programa de compresión y descompresión **LZW (Lempel–Ziv–Welch)** en Object Pascal.

Este manual tiene como objetivo ayudar a comprender:
- Cómo se inicializa y maneja el diccionario.
- Qué tipos de datos se declaran y por qué.
- Cómo se aplican estos tipos en las funciones de compresión y descompresión.

---

##  Tipos de Datos Definidos

El programa define varios tipos personalizados para mejorar la claridad y estructura del código:

```pascal
type
  TDictionary = array of string;
  TCode = Word;
  TCodeArray = array of TCode;
```

###  Descripción de cada tipo:

| Tipo | Descripción | Ejemplo |
|------|--------------|----------|
| **`TDictionary`** | Representa el **diccionario LZW**, una lista dinámica de cadenas (`string`) donde se almacenan combinaciones de caracteres encontradas. | `dict[0] = 'A'`, `dict[256] = 'AB'` |
| **`TCode`** | Es un **código numérico** (entero sin signo de 16 bits) que representa cada entrada del diccionario. | `65` para `'A'`, `256` para `'AB'` |
| **`TCodeArray`** | Arreglo dinámico que guarda los **códigos comprimidos** del texto original. | `[65, 66, 256, 68]` |

---

##  Inicialización del Diccionario

El algoritmo LZW comienza con un **diccionario inicial** que contiene todos los **caracteres ASCII (0–255)**.

```pascal
procedure TCompresorLZW.Inicializar(var dict: TDictionary);
var
  i: integer;
begin
  SetLength(dict, 256);
  for i := 0 to 255 do
    dict[i] := Chr(i);
end;
```

###  Explicación:
- `SetLength(dict, 256)` crea un arreglo de 256 posiciones.
- `Chr(i)` convierte el número `i` en su **carácter ASCII** equivalente.
- Ejemplo:  
  - `dict[65] = 'A'`
  - `dict[66] = 'B'`

Este diccionario inicial sirve como **base** para crear nuevas combinaciones durante la compresión.

---

##  Búsqueda en el Diccionario

La función `Buscar` revisa si una cadena ya existe dentro del diccionario.

```pascal
function TCompresorLZW.Buscar(const dict: TDictionary; const cadena: string): Integer;
var
  i: integer;
begin
  for i := 0 to High(dict) do
    if dict[i] = cadena then
      Exit(i);
  Result := -1;
end;
```

###  Explicación:
- Recorre todo el diccionario.
- Si encuentra la cadena, devuelve su **índice (código)**.
- Si no la encuentra, devuelve `-1`.

**Ejemplo:**
```pascal
Buscar(dict, 'AB')  →  256
Buscar(dict, 'Z')   →   90
Buscar(dict, 'XYZ') →  -1
```

---

##  Agregar Nuevas Entradas

Cuando se detecta una **nueva combinación de caracteres**, se agrega al diccionario.

```pascal
procedure TCompresorLZW.Agregar(var dict: TDictionary; const cadena: string);
begin
  SetLength(dict, Length(dict) + 1);
  dict[High(dict)] := cadena;
end;
```

###  Explicación:
- Se amplía el tamaño del arreglo en 1.
- Se añade la nueva cadena al final del diccionario.
- `High(dict)` devuelve el último índice disponible.

**Ejemplo:**
```pascal
Agregar(dict, 'AB')
Agregar(dict, 'BA')
```

Después de esto:
```
dict[256] = 'AB'
dict[257] = 'BA'
```

---

##  Compresión con LZW

Durante la **compresión**, el algoritmo:
1. Busca la cadena más larga existente en el diccionario.
2. La reemplaza por su código.
3. Agrega una nueva entrada combinando la cadena y el siguiente carácter.

---

##  Descompresión con LZW

En la **descompresión**, se reconstruye el texto original utilizando el mismo principio.

---

##  Cómo Guardar en `.txt` y `.bin`

Puedes agregar estas funciones al final del programa para guardar los resultados comprimidos:

```pascal
procedure GuardarComoTXT(const codigos: TCodeArray; const nombreArchivo: string);
var
  archivo: TextFile;
  i: Integer;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);
  for i := 0 to High(codigos) do
    Write(archivo, codigos[i], ' ');
  CloseFile(archivo);
end;

procedure GuardarComoBIN(const codigos: TCodeArray; const nombreArchivo: string);
var
  archivo: File of Word;
  i: Integer;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);
  for i := 0 to High(codigos) do
    Write(archivo, codigos[i]);
  CloseFile(archivo);
end;
```

**Ejemplo de uso:**
```pascal
GuardarComoTXT(codigosComprimidos, 'salida.txt');
GuardarComoBIN(codigosComprimidos, 'salida.bin');
```

---

## Resumen

| Elemento | Descripción |
|-----------|--------------|
| **Diccionario (TDictionary)** | Arreglo dinámico de cadenas que almacena patrones encontrados. |
| **Buscar** | Verifica si una cadena ya está en el diccionario. |
| **Agregar** | Añade una nueva combinación al diccionario. |
| **Comprimir** | Convierte el texto en una secuencia de códigos. |
| **Descomprimir** | Reconstruye el texto original a partir de los códigos. |
| **GuardarComoTXT / BIN** | Permiten exportar los códigos comprimidos a un archivo. |

---
