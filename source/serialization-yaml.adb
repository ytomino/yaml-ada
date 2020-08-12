with Ada.Unchecked_Deallocation;
package body Serialization.YAML is
	use type Ada.Strings.Unbounded.String_Access;
	use type Standard.YAML.Event_Type;
	
	Null_String : aliased String := "";
	
	procedure Free is
		new Ada.Unchecked_Deallocation (Serializer, Serializer_Access);
	procedure Free is new Ada.Unchecked_Deallocation (Reader, Reader_Access);
	procedure Free is new Ada.Unchecked_Deallocation (Writer, Writer_Access);
	
	procedure Free_And_Null (X : in out Ada.Strings.Unbounded.String_Access) is
	begin
		if X /= Null_String'Access then
			Ada.Strings.Unbounded.Free (X);
			X := Null_String'Access;
		end if;
	end Free_And_Null;
	
	procedure Handle (
		Object : not null access Reader;
		Event : in Standard.YAML.Event) is
	begin
		case Event.Event_Type is
			when Standard.YAML.Alias =>
				Object.Next_Kind := Value;
				Free_And_Null (Object.Next_Value);
				if Object.Level = 0 then
					Standard.YAML.Parse_Document_End (Object.Parser.all);
				end if;
			when Standard.YAML.Scalar =>
				Object.Next_Kind := Value;
				Free_And_Null (Object.Next_Value);
				Object.Next_Value := new String'(Event.Value.all);
				if Object.Level = 0 then
					Standard.YAML.Parse_Document_End (Object.Parser.all);
				end if;
			when Standard.YAML.Sequence_Start =>
				Object.Next_Kind := Enter_Sequence;
				Free_And_Null (Object.Next_Value);
				Object.Level := Object.Level + 1;
			when Standard.YAML.Sequence_End =>
				Object.Next_Kind := Leave_Sequence;
				Free_And_Null (Object.Next_Value);
				Object.Level := Object.Level - 1;
				if Object.Level = 0 then
					Standard.YAML.Parse_Document_End (Object.Parser.all);
				end if;
			when Standard.YAML.Mapping_Start =>
				Object.Next_Kind := Enter_Mapping;
				Free_And_Null (Object.Next_Value);
				Object.Level := Object.Level + 1;
			when Standard.YAML.Mapping_End =>
				Object.Next_Kind := Leave_Mapping;
				Free_And_Null (Object.Next_Value);
				Object.Level := Object.Level - 1;
				if Object.Level = 0 then
					Standard.YAML.Parse_Document_End (Object.Parser.all);
				end if;
			when Standard.YAML.No_Event
				| Standard.YAML.Stream_Start
				| Standard.YAML.Stream_End
				| Standard.YAML.Document_Start
				| Standard.YAML.Document_End =>
				Object.Next_Kind := End_Of_Stream;
				Free_And_Null (Object.Next_Value);
		end case;
	end Handle;
	
	procedure Advance_Start (
		Object : not null access Reader;
		Tag : in String)
	is
		Parsing_Entry : Standard.YAML.Parsing_Entry_Type;
	begin
		Standard.YAML.Parse (Object.Parser.all, Parsing_Entry);
		declare
			Event : Standard.YAML.Event
				renames Standard.YAML.Value (Parsing_Entry).Element.all;
		begin
			if Event.Tag /= null then
				declare
					Event_Tag : String renames Event.Tag.all;
					First : Positive := Event_Tag'First;
				begin
					if First <= Event_Tag'Last and then Event_Tag (First) = '!' then
						First := First + 1;
					end if;
					if Event_Tag (First .. Event_Tag'Last) /= Tag then
						raise Standard.YAML.Data_Error
							with """" & Event_Tag & """ is not expected tag (""" & Tag & """) .";
					end if;
				end;
			end if;
			Handle (Object, Event);
		end;
	end Advance_Start;
	
	procedure Emit_Name (Object : not null access Writer; Name : in String) is
	begin
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Scalar,
				Anchor => null,
				Tag => null,
				Value => Name'Unrestricted_Access,
				Plain_Implicit_Tag => True,
				Quoted_Implicit_Tag => True,
				Scalar_Style => Standard.YAML.Any));
	end Emit_Name;
	
	procedure Emit_Document_End (Object : not null access Writer) is
	begin
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Document_End, Implicit_Indicator => True));
	end Emit_Document_End;
	
	-- implementation
	
	overriding procedure Advance (
		Object : not null access Reader;
		Position : in State) is
	begin
		Free_And_Null (Object.Next_Name);
		if Position = In_Mapping then
			declare
				Parsing_Entry : Standard.YAML.Parsing_Entry_Type;
			begin
				Standard.YAML.Parse (Object.Parser.all, Parsing_Entry);
				declare
					Event : Standard.YAML.Event
						renames Standard.YAML.Value (Parsing_Entry).Element.all;
				begin
					if Event.Event_Type = Standard.YAML.Scalar then
						Object.Next_Name := new String'(Event.Value.all);
--						goto Process_Value;
					else
						Handle (Object, Event); -- complex mapping key
						return;
					end if;
				end;
			end;
		end if;
--	<<Process_Value>>
		declare
			Parsing_Entry : Standard.YAML.Parsing_Entry_Type;
		begin
			Standard.YAML.Parse (Object.Parser.all, Parsing_Entry);
			Handle (Object, Standard.YAML.Value (Parsing_Entry).Element.all);
		end;
	end Advance;
	
	overriding procedure Enter_Mapping (
		Object : not null access Writer;
		Name : in String)
	is
		Tag : Ada.Strings.Unbounded.String_Access := null;
		Implicit_Tag : Boolean;
	begin
		if Name /= "" then
			if Object.Tag /= null then
				raise Program_Error;
			end if;
			Emit_Name (Object, Name);
			Implicit_Tag := True;
		else
			if Object.Tag /= null then
				Tag := Object.Tag;
				Implicit_Tag := False;
			else
				Implicit_Tag := True;
			end if;
		end if;
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Mapping_Start,
				Anchor => null,
				Tag => Tag,
				Implicit_Tag => Implicit_Tag,
				Mapping_Style => Standard.YAML.Any));
		if Tag /= null then
			Object.Tag := null;
			Ada.Strings.Unbounded.Free (Tag);
		end if;
		Object.Level := Object.Level + 1;
	end Enter_Mapping;
	
	overriding procedure Enter_Sequence (
		Object : not null access Writer;
		Name : in String)
	is
		Tag : Ada.Strings.Unbounded.String_Access := null;
		Implicit_Tag : Boolean;
	begin
		if Name /= "" then
			if Object.Tag /= null then
				raise Program_Error;
			end if;
			Emit_Name (Object, Name);
			Implicit_Tag := True;
		else
			if Object.Tag /= null then
				Tag := Object.Tag;
				Implicit_Tag := False;
			else
				Implicit_Tag := True;
			end if;
		end if;
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Sequence_Start,
				Anchor => null,
				Tag => Tag,
				Implicit_Tag => Implicit_Tag,
				Sequence_Style => Standard.YAML.Any));
		if Tag /= null then
			Object.Tag := null;
			Ada.Strings.Unbounded.Free (Tag);
		end if;
		Object.Level := Object.Level + 1;
	end Enter_Sequence;
	
	overriding procedure Finalize (Object : in out Reference_Type) is
	begin
		Free (Object.Serializer_Body);
		if Object.Reader_Body /= null then
			if Object.Reader_Body.Next_Name /= Null_String'Access then
				Ada.Strings.Unbounded.Free (Object.Reader_Body.Next_Name);
			end if;
			if Object.Reader_Body.Next_Value /= Null_String'Access then
				Ada.Strings.Unbounded.Free (Object.Reader_Body.Next_Value);
			end if;
			Free (Object.Reader_Body);
		end if;
		if Object.Writer_Body /= null then
			Ada.Strings.Unbounded.Free (Object.Writer_Body.Tag);
			Free (Object.Writer_Body);
		end if;
	end Finalize;
	
	overriding procedure Leave_Mapping (Object : not null access Writer) is
	begin
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Mapping_End));
		Object.Level := Object.Level - 1;
		if Object.Level = 0 then
			Emit_Document_End (Object);
		end if;
	end Leave_Mapping;
	
	overriding procedure Leave_Sequence (Object : not null access Writer) is
	begin
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Sequence_End));
		Object.Level := Object.Level - 1;
		if Object.Level = 0 then
			Emit_Document_End (Object);
		end if;
	end Leave_Sequence;
	
	overriding function Next_Kind (Object : not null access Reader)
		return Stream_Element_Kind is
	begin
		return Object.Next_Kind;
	end Next_Kind;
	
	overriding function Next_Name (Object : not null access Reader)
		return not null access constant String is
	begin
		return Object.Next_Name;
	end Next_Name;
	
	overriding function Next_Value (Object : not null access Reader)
		return not null access constant String is
	begin
		return Object.Next_Value;
	end Next_Value;
	
	overriding procedure Put (
		Object : not null access Writer;
		Name : in String;
		Item : in String)
	is
		Tag : Ada.Strings.Unbounded.String_Access := null;
		Implicit_Tag : Boolean;
	begin
		if Name /= "" then
			if Object.Tag /= null then
				raise Program_Error;
			end if;
			Emit_Name (Object, Name);
			Implicit_Tag := True;
		else
			if Object.Tag /= null then
				Tag := Object.Tag;
				Implicit_Tag := False;
			else
				Implicit_Tag := True;
			end if;
		end if;
		Standard.YAML.Emit (
			Object.Emitter.all,
			(Event_Type => Standard.YAML.Scalar,
				Anchor => null,
				Tag => Tag,
				Value => Item'Unrestricted_Access,
				Plain_Implicit_Tag => Implicit_Tag,
				Quoted_Implicit_Tag => Implicit_Tag,
				Scalar_Style => Standard.YAML.Any));
		if Tag /= null then
			Object.Tag := null;
			Ada.Strings.Unbounded.Free (Tag);
		end if;
		if Object.Level = 0 then
			Emit_Document_End (Object);
		end if;
	end Put;
	
	function Reading (
		Parser : not null access Standard.YAML.Parser;
		Tag : String)
		return Reference_Type
	is
		pragma Suppress (Accessibility_Check);
		R : Reader_Access;
		S : Serializer_Access;
		In_Controlled : Boolean := False;
	begin
		R :=
			new Reader'(
				Parser => Parser,
				Next_Kind => End_Of_Stream,
				Next_Name => Null_String'Access,
				Next_Value => Null_String'Access,
				Level => 0);
		S := new Serializer'(Direction => Reading, Reader => R);
		return Result : constant Reference_Type :=
			(Ada.Finalization.Limited_Controlled
				with
					Serializer => S,
					Serializer_Body => S,
					Reader_Body => R,
					Writer_Body => null)
		do
			pragma Unreferenced (Result);
			In_Controlled := True;
			Standard.YAML.Parse_Document_Start (R.Parser.all);
			Advance_Start (R, Tag);
		end return;
	exception
		when others =>
			if not In_Controlled then
				if R /= null then
					if R.Next_Name /= Null_String'Access then
						Ada.Strings.Unbounded.Free (R.Next_Name);
					end if;
					if R.Next_Value /= Null_String'Access then
						Ada.Strings.Unbounded.Free (R.Next_Value);
					end if;
					Free (R);
				end if;
				Free (S);
			end if;
			raise;
	end Reading;
	
	function Writing (
		Emitter : not null access Standard.YAML.Emitter;
		Tag : String)
		return Reference_Type
	is
		pragma Suppress (Accessibility_Check);
		T : Ada.Strings.Unbounded.String_Access;
		W : Writer_Access;
		S : Serializer_Access;
		In_Controlled : Boolean := False;
	begin
		if Tag /= "" then
			T := new String'("!" & Tag);
		else
			T := null;
		end if;
		W := new Writer'(Emitter => Emitter, Tag => T, Level => 0);
		S := new Serializer'(Direction => Writing, Writer => W);
		return Result : constant Reference_Type :=
			(Ada.Finalization.Limited_Controlled
				with
					Serializer => S,
					Serializer_Body => S,
					Reader_Body => null,
					Writer_Body => W)
		do
			pragma Unreferenced (Result);
			In_Controlled := True;
			Standard.YAML.Emit (
				Emitter.all,
				(Event_Type => Standard.YAML.Document_Start,
					Implicit_Indicator => False,
					Version_Directive => null,
					Tag_Directives => null));
		end return;
	exception
		when others =>
			if not In_Controlled then
				Ada.Strings.Unbounded.Free (T);
				Free (W);
				Free (S);
			end if;
			raise;
	end Writing;
	
end Serialization.YAML;
