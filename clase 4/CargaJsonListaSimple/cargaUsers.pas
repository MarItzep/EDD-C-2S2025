program mostrarUsuarios;

uses
    crt, Classes, SysUtils, fpjson,jsonparser;

/// clase usuarios

type
    TUsuario = record
    id: Integer;
    nombre: String;
    usuario: String;
    password: String;
    email: String;
    telefono: String;
    end;
    // datos id ,nombre,usuario,password,email,telefono
// procedure cargarUsuarios(var usuarios: TList);
procedure MostrarUsers(u: TUsuario);
begin
    writeln('ID: ', u.id);
    writeln('Nombre: ', u.nombre);
    writeln('Usuario: ', u.usuario);
    writeln('Password: ', u.password);
    writeln('Email: ', u.email);
    writeln('Telefono: ', u.telefono);
    writeln('-----------------------');
end;

procedure cargarUsuarios(const archivo: String);
var 
    jsonData: TJSONData;
    jsonArray: TJSONArray;
    JSONObject: TJSONObject;
    U: TUsuario;
    jsonStr : TString;
    i: Integer;
begin
    if not FileExists(archivo) then
    begin
        writeln('El archivo no existe: ', archivo);
        exit;
    end;
    jsonStr := TString.Create;
    try
        jsonStr.LoadFromFile(archivo);
        jsonData := GetJSON(jsonStr);
        if not (jsonData is TJSONArray) then
        begin
            writeln('El contenido del archivo no es un arreglo JSON válido.');
            exit;
        end;

        jsonArray := TJSONArray(jsonData);
        for i := 0 to jsonArray.Count - 1 do
        begin
            JSONObject := jsonArray.Items[i] as TJSONObject;
            U.id := JSONObject.Get('id', 0);
            U.nombre := JSONObject.Get('nombre', '');
            U.usuario := JSONObject.Get('usuario', '');
            U.password := JSONObject.Get('password', '');
            U.email := JSONObject.Get('email', '');
            U.telefono := JSONObject.Get('telefono', '');

            writeln('ID: ', U.id);
            writeln('Nombre: ', U.nombre);
            writeln('Usuario: ', U.usuario);
            writeln('Password: ', U.password);
            writeln('Email: ', U.email);
            writeln('Telefono: ', U.telefono);
            writeln('-----------------------');
        end;
    finally
        jsonStr.Free;
    end;

    
    // ejecutar programa


    begin
        cargarUsuarios('C:\Users\LENOVO\Downloads\users.json');
    end;