with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;
with YAML;
procedure test_parser_error is
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
	use type YAML.Event_Type;
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
	-- error
	declare
		use Ada.Characters.Latin_1;
		Source : constant String :=
			"--- !ROOT-TAG" & LF
			& "[ complex_key ] : value1" & LF
			& "simple_key : value2" & LF
			& "- list_item" & LF; -- error
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
		R : aliased YAML.Parser := YAML.Create (Read'Access);
	begin
		declare
			Mark : constant YAML.Mark := YAML.Last_Error_Mark (R);
			Message : constant String := YAML.Last_Error_Message (R);
		begin
			if Mark.Index /= 0
				or else Mark.Line /= 0
				or else Mark.Column /= 0
				or else Message /= ""
			then
				raise Program_Error;
			end if;
		end;
		loop
			declare
				E : aliased YAML.Parsing_Entry_Type;
			begin
				YAML.Get (R, E);
				declare
					V : YAML.Event renames YAML.Value (E);
				begin
					Put (YAML.Event_Type'Image (V.Event_Type));
					if V.Event_Type = YAML.Scalar then
						Put (" """);
						Put (V.Value.all);
						Put ("""");
					end if;
					New_Line;
					exit when V.Event_Type = YAML.Document_End;
				end;
			end;
		end loop;
		raise Program_Error;
	exception
		when YAML.Data_Error =>
			declare
				Mark : constant YAML.Mark := YAML.Last_Error_Mark (R);
			begin
				Put ("(");
				Put (Integer'Image (Mark.Index));
				Put (")");
				Put (Integer'Image (Mark.Line));
				Put (":");
				Put (Integer'Image (Mark.Column));
				Put (": ");
				Put (YAML.Last_Error_Message (R)); -- "did not find expected key"
				New_Line;
				if Mark.Index /= 59
					or else Mark.Line /= 3
					or else Mark.Column /= 0 -- each index is 0 origin
				then
					raise Program_Error;
				end if;
			end;
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_parser_error;
