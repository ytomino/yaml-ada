with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Serialization.YAML;
with YAML.Streams;
procedure test_serialize is
	Verbose : Boolean := False;
	procedure Put (Item : in String) is
	begin
		if Verbose then
			Ada.Text_IO.Put (Item);
		end if;
	end Put;
	procedure New_Line is
	begin
		if Verbose then
			Ada.Text_IO.New_Line;
		end if;
	end New_Line;
	Test_File_Name : constant String :=
		Ada.Directories.Compose (
			Ada.Environment_Variables.Value ("TMPDIR", Default => "/tmp"),
			"test_serialize.yaml");
	type Mapping_Method_Type is (By_Closure, By_Iterator);
	type Nested_Map is record
		A : Integer;
	end record;
	type T is record
		X : Ada.Strings.Unbounded.Unbounded_String;
		Y : Boolean;
		Z : Nested_Map;
	end record;
begin
	-- options
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			A : constant String := Ada.Command_Line.Argument (I);
		begin
			if A = "--verbose" then
				Verbose := True;
			else
				Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "unknown option: " & A);
				Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
				return;
			end if;
		end;
	end loop;
	-- check the closeure-style and the iterator-style
	for Mapping_Method in Mapping_Method_Type loop
		declare
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
				case Mapping_Method is
					when By_Closure =>
						Serialization.IO (S, Name, Process'Access);
					when By_Iterator =>
						for I in Serialization.IO (S, Name) loop
							Process;
						end loop;
				end case;
			end IO;
			procedure IO (S : not null access Serialization.Serializer; Var : in out T) is
				procedure Process is
				begin
					Serialization.IO (S, "X", Var.X);
					Serialization.IO (S, "Y", Var.Y);
					IO (S, "Z", Var.Z);
				end Process;
			begin
				case Mapping_Method is
					when By_Closure =>
						Serialization.IO (S, Process'Access);
					when By_Iterator =>
						for I in Serialization.IO (S) loop
							Process;
						end loop;
				end case;
			end IO;
			Root_Tag : constant String := "ROOT-TAG";
			Data : T := (
				X =>
					Ada.Strings.Unbounded.To_Unbounded_String (
						Mapping_Method_Type'Image (Mapping_Method)),
				Y => True,
				Z => (A => 100));
		begin
			Put ("**** " & Mapping_Method_Type'Image (Mapping_Method) & " ****");
			New_Line;
			-- emitter
			declare
				W : aliased YAML.Emitter := YAML.Create (Put'Access);
			begin
				IO (Serialization.YAML.Writing (W'Access, Root_Tag).Serializer, Data);
				YAML.Finish (W);
			end;
			declare
				File : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Create (File, Name => Test_File_Name);
				declare
					W : aliased YAML.Emitter :=
						YAML.Streams.Create (Ada.Streams.Stream_IO.Stream (File));
				begin
					Put ("Writing...");
					IO (Serialization.YAML.Writing (W'Access, Root_Tag).Serializer, Data);
					YAML.Finish (W);
					Put (" ok");
					New_Line;
				end;
				Ada.Streams.Stream_IO.Close (File);
			end;
			-- parser
			declare
				use Ada.Characters.Latin_1;
				Source : constant String :=
					"--- !ROOT-TAG" & LF
					& "? [ complex_key ] : value" & LF
					& "X: " & Mapping_Method_Type'Image (Mapping_Method) & LF
					& "Y: TRUE" & LF
					& "Z:" & LF
					& " A: 100" & LF;
				Source_First : Positive := Source'First;
				procedure Read (Item : out String; Last : out Natural) is
					R : constant Integer :=
						Integer'Min (Item'Length - 1, Source'Last - Source_First);
					Source_Last : constant Natural := Source_First + R;
				begin
					Last := Source'First + R;
					Item (Item'First .. Last) := Source (Source_First .. Source_Last);
					Source_First := Source_Last + 1;
				end Read;
				Data2 : T := (
					X => Ada.Strings.Unbounded.Null_Unbounded_String,
					Y => False,
					Z => (A => 0));
			begin
				declare
					R : aliased YAML.Parser := YAML.Create (Read'Access);
				begin
					IO (Serialization.YAML.Reading (R'Access, Root_Tag).Serializer, Data2);
					YAML.Finish (R);
				end;
				if Data2 /= Data then
					raise Program_Error;
				end if;
			end;
			declare
				File : Ada.Streams.Stream_IO.File_Type;
				Data2 : T := (
					X => Ada.Strings.Unbounded.Null_Unbounded_String,
					Y => False,
					Z => (A => 0));
			begin
				Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File,
					Name => Test_File_Name);
				declare
					R : aliased YAML.Parser :=
						YAML.Streams.Create (Ada.Streams.Stream_IO.Stream (File));
				begin
					Put ("Reading...");
					IO (Serialization.YAML.Reading (R'Access, Root_Tag).Serializer, Data2);
					YAML.Finish (R);
					Put (" ok");
					New_Line;
				end;
				Ada.Streams.Stream_IO.Close (File);
				if Data2 /= Data then
					raise Program_Error;
				end if;
			end;
		end;
	end loop;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_serialize;
