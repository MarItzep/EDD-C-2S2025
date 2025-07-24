program TiposDatosPrimitivos;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

// =====================================================
// TIPOS DE DATOS PRIMITIVOS
// =====================================================

procedure EjemplosTiposPrimitivos;
var
  // Tipos enteros
  entero: Integer;
  enteroCorto: SmallInt;
  enteroLargo: LongInt;
  enteroSinSigno: Cardinal;
  enteroByte: Byte;
  enteroWord: Word;
  
  // Tipos reales
  real1: Real;
  doble: Double;
  extendido: Extended;
  simple: Single;
  
  // Tipos lógicos
  booleano: Boolean;
  
  // Tipos caracter y cadena
  caracter: Char;
  cadena: String;
  cadenaCorta: String[50];
  
begin
  WriteLn('=== TIPOS DE DATOS PRIMITIVOS ===');
  WriteLn;
  
  // ===== TIPOS ENTEROS =====
  WriteLn('TIPOS ENTEROS:');
  WriteLn('--------------');
  
  entero := 42;
  enteroCorto := 1000;
  enteroLargo := 2000000;
  enteroSinSigno := 4000000000;
  enteroByte := 255;
  enteroWord := 65535;
  
  WriteLn('Integer (', SizeOf(entero), ' bytes): ', entero);
  WriteLn('SmallInt (', SizeOf(enteroCorto), ' bytes): ', enteroCorto);
  WriteLn('LongInt (', SizeOf(enteroLargo), ' bytes): ', enteroLargo);
  WriteLn('Cardinal (', SizeOf(enteroSinSigno), ' bytes): ', enteroSinSigno);
  WriteLn('Byte (', SizeOf(enteroByte), ' bytes): ', enteroByte);
  WriteLn('Word (', SizeOf(enteroWord), ' bytes): ', enteroWord);
  WriteLn;
  
  // Operaciones con enteros
  WriteLn('OPERACIONES CON ENTEROS:');
  WriteLn('Suma: ', entero, ' + 10 = ', entero + 10);
  WriteLn('Resta: ', entero, ' - 5 = ', entero - 5);
  WriteLn('Multiplicación: ', entero, ' * 2 = ', entero * 2);
  WriteLn('División entera: ', entero, ' div 3 = ', entero div 3);
  WriteLn('Módulo: ', entero, ' mod 5 = ', entero mod 5);
  WriteLn;
  
  // ===== TIPOS REALES =====
  WriteLn('TIPOS REALES:');
  WriteLn('-------------');
  
  real1 := 3.14159;
  doble := 2.718281828;
  extendido := 1.23456789012345;
  simple := 1.414;
  
  WriteLn('Real (', SizeOf(real1), ' bytes): ', real1:0:5);
  WriteLn('Double (', SizeOf(doble), ' bytes): ', doble:0:9);
  WriteLn('Extended (', SizeOf(extendido), ' bytes): ', extendido:0:11);
  WriteLn('Single (', SizeOf(simple), ' bytes): ', simple:0:3);
  WriteLn;
  
  // Operaciones con reales
  WriteLn('OPERACIONES CON REALES:');
  WriteLn('Suma: ', real1:0:3, ' + 1.5 = ', (real1 + 1.5):0:3);
  WriteLn('Resta: ', real1:0:3, ' - 0.14159 = ', (real1 - 0.14159):0:3);
  WriteLn('Multiplicación: ', real1:0:3, ' * 2 = ', (real1 * 2):0:3);
  WriteLn('División: ', real1:0:3, ' / 2 = ', (real1 / 2):0:3);
  WriteLn('Raíz cuadrada de 16: ', Sqrt(16):0:1);
  WriteLn('Potencia 2^3: ', Power(2, 3):0:0);
  WriteLn;
  
  // ===== TIPOS LÓGICOS =====
  WriteLn('TIPOS LÓGICOS:');
  WriteLn('--------------');
  
  booleano := True;
  WriteLn('Boolean (', SizeOf(booleano), ' bytes): ', booleano);
  WriteLn('Valor contrario: ', not booleano);
  WriteLn;
  
  // Operaciones lógicas
  WriteLn('OPERACIONES LÓGICAS:');
  WriteLn('True AND False = ', True and False);
  WriteLn('True OR False = ', True or False);
  WriteLn('True XOR False = ', True xor False);
  WriteLn('NOT True = ', not True);
  WriteLn;
  
  // Comparaciones
  WriteLn('COMPARACIONES:');
  WriteLn('42 > 30: ', 42 > 30);
  WriteLn('42 < 30: ', 42 < 30);
  WriteLn('42 = 42: ', 42 = 42);
  WriteLn('42 <> 30: ', 42 <> 30);
  WriteLn('42 >= 42: ', 42 >= 42);
  WriteLn('42 <= 41: ', 42 <= 41);
  WriteLn;
  
  // ===== TIPOS CARACTER Y CADENA =====
  WriteLn('TIPOS CARACTER Y CADENA:');
  WriteLn('------------------------');
  
  caracter := 'A';
  cadena := 'Hola Mundo desde Pascal';
  cadenaCorta := 'Cadena limitada a 50 caracteres';
  
  WriteLn('Char (', SizeOf(caracter), ' bytes): ', caracter);
  WriteLn('String: ', cadena);
  WriteLn('String[50]: ', cadenaCorta);
  WriteLn('Longitud de cadena: ', Length(cadena));
  WriteLn('Longitud de cadena corta: ', Length(cadenaCorta));
  WriteLn;
  
  // Operaciones con caracteres y cadenas
  WriteLn('OPERACIONES CON CARACTERES Y CADENAS:');
  WriteLn('Código ASCII de ''A'': ', Ord('A'));
  WriteLn('Caracter del código 66: ', Chr(66));
  WriteLn('Mayúscula de ''a'': ', UpCase('a'));
  WriteLn('Concatenación: ', 'Hola' + ' ' + 'Mundo');
  WriteLn('Subcadena (pos 6-10): ', Copy(cadena, 6, 5));
  WriteLn('Posición de "Mundo": ', Pos('Mundo', cadena));
  WriteLn('Cadena en mayúsculas: ', UpperCase(cadena));
  WriteLn('Cadena en minúsculas: ', LowerCase(cadena));
  WriteLn;
  
  // ===== CONVERSIONES DE TIPOS =====
  WriteLn('CONVERSIONES DE TIPOS:');
  WriteLn('----------------------');
  
  WriteLn('Entero a cadena: ', IntToStr(entero));
  WriteLn('Real a cadena: ', FloatToStr(real1));
  WriteLn('Cadena "123" a entero: ', StrToInt('123'));
  WriteLn('Cadena "3.14" a real: ', StrToFloat('3.14'):0:2);
  WriteLn('Booleano a cadena: ', BoolToStr(booleano, True));
  WriteLn;
  
  // ===== RANGOS DE VALORES =====
  WriteLn('RANGOS DE VALORES:');
  WriteLn('------------------');
  
  WriteLn('Rango de Byte: 0 a ', High(Byte));
  WriteLn('Rango de SmallInt: ', Low(SmallInt), ' a ', High(SmallInt));
  WriteLn('Rango de Integer: ', Low(Integer), ' a ', High(Integer));
  WriteLn('Rango de Word: ', Low(Word), ' a ', High(Word));
  WriteLn('Rango de Cardinal: ', Low(Cardinal), ' a ', High(Cardinal));
  WriteLn;
  
  // ===== CONSTANTES ESPECIALES =====
  WriteLn('CONSTANTES ESPECIALES:');
  WriteLn('----------------------');
  
  WriteLn('PI: ', Pi:0:10);
  WriteLn('Número E: ', Exp(1):0:10);
  WriteLn('Infinito: ', 1.0/0.0);
  WriteLn('Valor máximo Integer: ', MaxInt);
  WriteLn;
end;

// =====================================================
// PROGRAMA PRINCIPAL
// =====================================================

begin
  WriteLn('EJEMPLOS DE TIPOS DE DATOS PRIMITIVOS EN OBJECT PASCAL');
  WriteLn('======================================================');
  WriteLn;
  
  EjemplosTiposPrimitivos;
  
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.