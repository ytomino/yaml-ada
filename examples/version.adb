with Ada.Text_IO;
with YAML;
procedure version is
begin
	Ada.Text_IO.Put_Line (YAML.Version);
end version;
