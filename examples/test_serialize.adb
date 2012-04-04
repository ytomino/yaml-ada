with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Serialization.YAML;
with YAML.Streams;
procedure test_serialize is
	Test_File_Name : constant String := "test_serialize.yaml";
	type Nested_Map is record
		A : Integer;
	end record;
	procedure IO (
		S : not null access Serialization.Serializer;
		Name : String;
		Var : in out Nested_Map)
	is
		procedure Process is
		begin
			Serialization.IO (S, "A", Var.A);
		end Process;
	begin
		Serialization.IO (S, Name, Process'Access);
	end IO;
	type T is record
		X : Ada.Strings.Unbounded.Unbounded_String;
		Y : Boolean;
		Z : Nested_Map;
	end record;
	procedure IO (S : not null access Serialization.Serializer; Var : in out T) is
		procedure Process is
		begin
			Serialization.IO (S, "X", Var.X);
			Serialization.IO (S, "Y", Var.Y);
			IO (S, "Z", Var.Z);
		end Process;
	begin
		Serialization.IO (S, Process'Access);
	end IO;
	Root_Tag : constant String := "ROOT-TAG";
	Data : T := (
		X => Ada.Strings.Unbounded.To_Unbounded_String ("XYZ"),
		Y => True,
		Z => (A => 100));
begin
	declare
		W : aliased YAML.Emitter := YAML.Create (Ada.Text_IO.Put'Access);
	begin
		YAML.Emit (W, (Event_Type => YAML.Stream_Start, Encoding => YAML.Any));
		IO (Serialization.YAML.Writing (W'Access, Root_Tag).Serializer, Data);
		YAML.Emit (W, (Event_Type => YAML.Stream_End));
		YAML.Flush (W);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (
			File,
			Name => Test_File_Name);
		declare
			W : aliased YAML.Emitter := YAML.Streams.Create (
				Ada.Streams.Stream_IO.Stream (File));
		begin
			Ada.Text_IO.Put ("Writing...");
			YAML.Emit (W, (Event_Type => YAML.Stream_Start, Encoding => YAML.Any));
			IO (Serialization.YAML.Writing (W'Access, Root_Tag).Serializer, Data);
			YAML.Emit (W, (Event_Type => YAML.Stream_End));
			YAML.Flush (W);
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
	end;
	declare
		File : Ada.Streams.Stream_IO.File_Type;
		Data2 : T := (
			X => Ada.Strings.Unbounded.Null_Unbounded_String,
			Y => False,
			Z => (A => 0));
	begin
		Ada.Streams.Stream_IO.Open (
			File,
			Ada.Streams.Stream_IO.In_File,
			Name => Test_File_Name);
		declare
			R : aliased YAML.Parser := YAML.Streams.Create (
				Ada.Streams.Stream_IO.Stream (File));
		begin
			Ada.Text_IO.Put ("Reading...");
			YAML.Parse_Stream_Start (R);
			IO (Serialization.YAML.Reading (R'Access, Root_Tag).Serializer, Data2);
			YAML.Parse_Stream_End (R);
			Ada.Text_IO.Put_Line (" ok");
		end;
		Ada.Streams.Stream_IO.Close (File);
		declare
			W : aliased YAML.Emitter := YAML.Create (Ada.Text_IO.Put'Access);
		begin
			YAML.Emit (W, (Event_Type => YAML.Stream_Start, Encoding => YAML.Any));
			IO (Serialization.YAML.Writing (W'Access, Root_Tag).Serializer, Data2);
			YAML.Emit (W, (Event_Type => YAML.Stream_End));
			YAML.Flush (W);
		end;
		if Data2 /= Data then
			raise Program_Error;
		end if;
	end;
end test_serialize;
