program listacircular;
uses crt;

type
  TDato = Integer;

  PNodo = ^TNodo;
  TNodo = record
    dato: TDato;
    siguiente: PNodo;
  end;

  TLista = record
    cabeza: PNodo;
    tamano: Integer;
  end;

procedure InicializarLista(var lista: TLista);
begin
  lista.cabeza := nil;
  lista.tamano := 0;
end;

// Insertar al inicio
procedure InsertarInicio(var lista: TLista; valor: TDato);
var
  nuevoNodo, actual: PNodo;
begin
  new(nuevoNodo);
  nuevoNodo^.dato := valor;

  if lista.cabeza = nil then
  begin
    nuevoNodo^.siguiente := nuevoNodo;  // Apunta a sí mismo
    lista.cabeza := nuevoNodo;
  end
  else
  begin
    actual := lista.cabeza;
    // Recorrer hasta el último nodo
    while actual^.siguiente <> lista.cabeza do
      actual := actual^.siguiente;

    nuevoNodo^.siguiente := lista.cabeza;
    actual^.siguiente := nuevoNodo;
    lista.cabeza := nuevoNodo;
  end;

  Inc(lista.tamano);
end;

// Insertar al final
procedure InsertarAlFinal(var lista: TLista; valor: TDato);
var
  nuevoNodo, actual: PNodo;
begin
  new(nuevoNodo);
  nuevoNodo^.dato := valor;

  if lista.cabeza = nil then
  begin
    nuevoNodo^.siguiente := nuevoNodo;
    lista.cabeza := nuevoNodo;
  end
  else
  begin
    actual := lista.cabeza;
    while actual^.siguiente <> lista.cabeza do
      actual := actual^.siguiente;

    actual^.siguiente := nuevoNodo;
    nuevoNodo^.siguiente := lista.cabeza;
  end;

  Inc(lista.tamano);
end;

// Eliminar nodo
function EliminarNodo(var lista: TLista; valor: TDato): Boolean;
var
  actual, anterior: PNodo;
begin
  Result := False;
  if lista.cabeza = nil then Exit;

  actual := lista.cabeza;
  anterior := nil;

  repeat
    if actual^.dato = valor then
    begin
      if anterior = nil then
      begin
        // Nodo a eliminar es la cabeza
        if actual^.siguiente = lista.cabeza then
        begin
          // Solo un nodo
          Dispose(actual);
          lista.cabeza := nil;
        end
        else
        begin
          // Más de un nodo
          anterior := lista.cabeza;
          while anterior^.siguiente <> lista.cabeza do
            anterior := anterior^.siguiente;
          anterior^.siguiente := actual^.siguiente;
          lista.cabeza := actual^.siguiente;
          Dispose(actual);
        end;
      end
      else
      begin
        anterior^.siguiente := actual^.siguiente;
        Dispose(actual);
      end;

      Dec(lista.tamano);
      Result := True;
      Exit;
    end;

    anterior := actual;
    actual := actual^.siguiente;
  until actual = lista.cabeza;
end;

// Buscar elemento
function Buscar(lista: TLista; valor: TDato): Boolean;
var
  actual: PNodo;
begin
  Result := False;
  if lista.cabeza = nil then Exit;

  actual := lista.cabeza;
  repeat
    if actual^.dato = valor then
      Exit(True);
    actual := actual^.siguiente;
  until actual = lista.cabeza;
end;

// Mostrar lista
procedure MostrarLista(lista: TLista);
var
  actual: PNodo;
begin
  if lista.cabeza = nil then
  begin
    Writeln('Lista vacía');
    Exit;
  end;

  Write('Lista: [');
  actual := lista.cabeza;
  repeat
    Write(actual^.dato);
    if actual^.siguiente <> lista.cabeza then
      Write(', ');
    actual := actual^.siguiente;
  until actual = lista.cabeza;
  Writeln(']');
  Writeln('Tamaño: ', lista.tamano);
end;

// Liberar lista
procedure LiberarLista(var lista: TLista);
var
  actual, temp: PNodo;
begin
  if lista.cabeza = nil then Exit;

  actual := lista.cabeza^.siguiente;
  while actual <> lista.cabeza do
  begin
    temp := actual;
    actual := actual^.siguiente;
    Dispose(temp);
  end;

  Dispose(lista.cabeza);
  lista.cabeza := nil;
  lista.tamano := 0;
end;

// Programa principal
var
  miLista: TLista;
begin
  InicializarLista(miLista);

  InsertarAlFinal(miLista, 10);
  InsertarAlFinal(miLista, 20);
  InsertarInicio(miLista, 5);
  InsertarAlFinal(miLista, 30);

  Writeln('Contenido de la lista:');
  MostrarLista(miLista);

  if Buscar(miLista, 20) then
    Writeln('20 está en la lista')
  else
    Writeln('20 NO está en la lista');

  if EliminarNodo(miLista, 10) then
    Writeln('10 eliminado correctamente')
  else
    Writeln('10 no encontrado');

  MostrarLista(miLista);

  LiberarLista(miLista);

  readkey;
end.

