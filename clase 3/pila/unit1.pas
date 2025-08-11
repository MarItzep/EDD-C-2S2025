unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Process;
// Se define un registro (estructura) que representa cada elemento de la pila
type
  PNodo = ^TNodo;
  TNodo = record
    valor: string;
    siguiente: PNodo;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ApilarBtn: TButton;
    DesapilarBtn: TButton;
    EditValor: TEdit;
    MemoPila: TMemo;
    MostrarGraficaBtn: TButton;
    ImagePila: TImage;
    procedure ApilarBtnClick(Sender: TObject);
    procedure DesapilarBtnClick(Sender: TObject);
    procedure MostrarGraficaBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    cima: PNodo;
    procedure MostrarPila;
    procedure GenerarArchivoDOT(const ArchivoDOT: string);
    procedure EjecutarGraphviz(const ArchivoDOT, ArchivoPNG: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// Inicialización
procedure TForm1.FormCreate(Sender: TObject);
begin
  cima := nil;
  MemoPila.Clear;
  ImagePila.Picture := nil;
end;

// Apilar un nuevo valor
procedure TForm1.ApilarBtnClick(Sender: TObject);
var
  nuevoNodo: PNodo;
begin
  if Trim(EditValor.Text) = '' then
  begin
    ShowMessage('Ingrese un valor para apilar.');
    Exit;
  end;

  New(nuevoNodo);
  nuevoNodo^.valor := EditValor.Text;
  nuevoNodo^.siguiente := cima;
  cima := nuevoNodo;

  EditValor.Clear;
  MostrarPila;
end;

// Desapilar el valor superior
procedure TForm1.DesapilarBtnClick(Sender: TObject);
var
  temp: PNodo;
begin
  if cima = nil then
  begin
    ShowMessage('La pila está vacía.');
    Exit;
  end;

  temp := cima;
  cima := cima^.siguiente;
  Dispose(temp);

  MostrarPila;
end;

// Mostrar contenido de la pila en Memo
procedure TForm1.MostrarPila;
var
  actual: PNodo;
begin
  MemoPila.Clear;
  actual := cima;
  if actual = nil then
  begin
    MemoPila.Lines.Add('Pila vacía.');
    Exit;
  end;

  MemoPila.Lines.Add('==========PILA==========:');
  while actual <> nil do
  begin
    MemoPila.Lines.Add(actual^.valor);
    actual := actual^.siguiente;
  end;
end;

// Crear archivo DOT para Graphviz
procedure TForm1.GenerarArchivoDOT(const ArchivoDOT: string);
var
  f: TextFile;
  actual: PNodo;
  nodos, enlaces: TStringList;
begin
  AssignFile(f, ArchivoDOT);
  Rewrite(f);

  Writeln(f, 'digraph Pila {');
  Writeln(f, '  rankdir=LR;'); // Orientación izquierda a derecha
  Writeln(f, '  node [shape=record, style=filled, fillcolor=lightblue];');

  nodos := TStringList.Create;
  enlaces := TStringList.Create;
  try
    actual := cima;
    while actual <> nil do
    begin
      nodos.Add(Format('  "%s" [label="%s"];', [actual^.valor, actual^.valor]));
      if actual^.siguiente <> nil then
        enlaces.Add(Format('  "%s" -> "%s";', [actual^.valor, actual^.siguiente^.valor]))
      else
        enlaces.Add(Format('  "%s" -> nil [style=dashed];', [actual^.valor]));
      actual := actual^.siguiente;
    end;

    Writeln(f, '  nil [shape=point];'); // Nodo final nil

    Writeln(f, nodos.Text);
    Writeln(f, enlaces.Text);
  finally
    nodos.Free;
    enlaces.Free;
  end;

  Writeln(f, '}');
  CloseFile(f);
end;

// Graphviz para crear PNG desde DOT
procedure TForm1.EjecutarGraphviz(const ArchivoDOT, ArchivoPNG: string);
var
  proc: TProcess;
begin
  proc := TProcess.Create(nil);
  try
    proc.Executable := 'dot'; //
    proc.Parameters.Add('-Tpng');
    proc.Parameters.Add(ArchivoDOT);
    proc.Parameters.Add('-o');
    proc.Parameters.Add(ArchivoPNG);
    proc.Options := proc.Options + [poWaitOnExit, poUsePipes];
    proc.Execute;
  finally
    proc.Free;
  end;
end;

// Botón para generar y mostrar gráfica
procedure TForm1.MostrarGraficaBtnClick(Sender: TObject);
const
  ArchivoDOT = 'pila.dot';
  ArchivoPNG = 'pila.png';
begin
  if cima = nil then
  begin
    ShowMessage('La pila está vacía, no hay nada que graficar.');
    Exit;
  end;

  GenerarArchivoDOT(ArchivoDOT);
  EjecutarGraphviz(ArchivoDOT, ArchivoPNG);

  if FileExists(ArchivoPNG) then
    ImagePila.Picture.LoadFromFile(ArchivoPNG)
  else
    ShowMessage('Error al generar la imagen con Graphviz.');
end;

end.

