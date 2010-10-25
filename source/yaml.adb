with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with C.stdlib;
with C.string;
package body YAML is
	use type C.signed_int;
	use type C.ptrdiff_t;
	use type C.size_t;
	use type C.yaml.yaml_char_t_ptr;
	use type C.yaml.yaml_event_type_t;
	use type C.yaml.yaml_tag_directive_t_ptr;
	use type C.yaml.yaml_version_directive_t_ptr;
	
	-- local
	
	type yaml_tag_directive_t_array is array (C.size_t range <>) of
		aliased C.yaml.yaml_tag_directive_t;
	type yaml_tag_directive_t_array_access is access yaml_tag_directive_t_array;
	procedure Free is new Ada.Unchecked_Deallocation (
		yaml_tag_directive_t_array,
		yaml_tag_directive_t_array_access);
	
	function strdup (S : access constant String) return C.yaml.yaml_char_t_ptr is
	begin
		if S = null then
			return null;
		else
			declare
				function Cast is new Ada.Unchecked_Conversion (
					C.void_ptr,
					C.yaml.yaml_char_t_ptr);
				function Cast is new Ada.Unchecked_Conversion (
					C.yaml.yaml_char_t_ptr,
					C.ptrdiff_t);
				function Cast is new Ada.Unchecked_Conversion (
					C.ptrdiff_t,
					C.yaml.yaml_char_t_ptr);
				Length : constant C.size_t := S'Length;
				p : constant C.void_ptr := C.stdlib.malloc (Length + 1);
				Dummy : C.void_ptr;
				pragma Unreferenced (Dummy);
			begin
				Dummy := C.string.memmove (p, C.void_const_ptr (S.all'Address), Length);
				Cast (Cast (Cast (p)) + C.ptrdiff_t (Length)).all :=
					C.yaml.yaml_char_t'Val (0);
				return Cast (p);
			end;
		end if;
	end strdup;
	
	type String_Access is access String;
	procedure Free is new Ada.Unchecked_Deallocation (
		String,
		String_Access);
	
	function To_String (S : not null access constant C.char) return String is
		Result : String (1 .. Natural (C.string.strlen (S)));
		for Result'Address use S.all'Address;
	begin
		return Result;
	end To_String;
	
	function New_String (S : C.yaml.yaml_char_t_ptr) return String_Access is
	begin
		if S = null then
			return null;
		else
			declare
				function Cast is new Ada.Unchecked_Conversion (
					C.yaml.yaml_char_t_ptr,
					C.char_ptr);
				Length : constant Natural := Natural (C.string.strlen (Cast (S)));
				Dummy : C.void_ptr;
				pragma Unreferenced (Dummy);
			begin
				return Result : constant String_Access := new String (1 .. Length) do
					Dummy := C.string.memmove (
						C.void_ptr (Result.all'Address),
						C.void_const_ptr (S.all'Address),
						C.size_t (Length));
				end return;
			end;
		end if;
	end New_String;
	
	type Version_Directive_Access is access all YAML.Version_Directive;
	
	type Tag_Directive_Array_Access is access Tag_Directive_Array;
	procedure Free is new Ada.Unchecked_Deallocation (
		Tag_Directive_Array,
		Tag_Directive_Array_Access);
	
	function Read_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t;
		size_read : access C.size_t)
		return C.signed_int;
	pragma Convention (C, Read_Handler);
	function Read_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t;
		size_read : access C.size_t)
		return C.signed_int
	is
		procedure Input (Item : out String; Last : out Natural);
		pragma Import (Ada, Input);
		for Input'Address use System.Address (data);
		Ada_Data : String (1 .. Natural (size));
		for Ada_Data'Address use buffer.all'Address;
		Last : Natural;
	begin
		Input (Ada_Data, Last);
		size_read.all := C.size_t (Last);
		return 1;
	end Read_Handler;
	
	function Write_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t)
		return C.signed_int;
	pragma Convention (C, Write_Handler);
	function Write_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t)
		return C.signed_int
	is
		procedure Output (Item : in String);
		pragma Import (Ada, Output);
		for Output'Address use System.Address (data);
		Ada_Data : String (1 .. Natural (size));
		for Ada_Data'Address use buffer.all'Address;
	begin
		Output (Ada_Data);
		return 1;
	end Write_Handler;
	
	procedure Parse_Expection (
		Object : in out Parser;
		Expected : C.yaml.yaml_event_type_t)
	is
		Ev : aliased C.yaml.yaml_event_t;
		T : C.yaml.yaml_event_type_t;
	begin
		if C.yaml.yaml_parser_parse (Object.Raw'Access, Ev'Access) = 0 then
			Raise_Error (Object.Raw.error, Object.Raw.problem, Object.Raw.mark'Access);
		end if;
		T := Ev.F_type;
		C.yaml.yaml_event_delete (Ev'Access);
		if T /= Expected then
			raise Status_Error;
		end if;
	end Parse_Expection;
	
	-- body
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural))
		return Parser is
	begin
		return Result : Parser do
			if C.yaml.yaml_parser_initialize (Result.Raw'Access) = 0 then
				Raise_Error (Result.Raw.error, Result.Raw.problem, null);
			end if;
			C.yaml.yaml_parser_set_input (
				Result.Raw'Access,
				Read_Handler'Access,
				C.void_ptr (Input.all'Address));
		end return;
	end Create;
	
	function Create (
		Output : not null access procedure (Item : in String))
		return Emitter is
	begin
		return Result : Emitter do
			if C.yaml.yaml_emitter_initialize (Result.Raw'Access) = 0 then
				Raise_Error (Result.Raw.error, Result.Raw.problem, null);
			end if;
			C.yaml.yaml_emitter_set_output (
				Result.Raw'Access,
				Write_Handler'Access,
				C.void_ptr (Output.all'Address));
		end return;
	end Create;
	
	procedure Emit (Object : in out Emitter; Event : in YAML.Event) is
		Ev : aliased C.yaml.yaml_event_t;
	begin
		case Event.Event_Type is
			when No_Event =>
				raise Constraint_Error;
			when Stream_Start =>
				if C.yaml.yaml_stream_start_event_initialize (
					Ev'Access,
					C.yaml.yaml_encoding_t'Enum_Val (
					YAML.Encoding'Enum_Rep (Event.Encoding))) = 0
				then
					raise Storage_Error; -- maybe ...
				end if;
			when Stream_End =>
				if C.yaml.yaml_stream_end_event_initialize (Ev'Access) = 0 then
					raise Storage_Error; -- maybe ...
				end if;
			when Document_Start =>
				declare
					Version_Directive : C.yaml.yaml_version_directive_t_ptr;
					VD_Body : aliased C.yaml.yaml_version_directive_t;
					Tag_Directives_Start : C.yaml.yaml_tag_directive_t_ptr;
					Tag_Directives_End : C.yaml.yaml_tag_directive_t_ptr;
					TD_Length : C.size_t;
					TD_Body : yaml_tag_directive_t_array_access;
					Error : Boolean;
				begin
					if Event.Version_Directive = null then
						Version_Directive := null;
					else
						VD_Body.major := C.signed_int (Event.Version_Directive.Major);
						VD_Body.major := C.signed_int (Event.Version_Directive.Minor);
						Version_Directive := VD_Body'Unchecked_Access;
					end if;
					if Event.Tag_Directives = null
						or else Event.Tag_Directives'Length = 0
					then
						Tag_Directives_Start := null;
						Tag_Directives_End := null;
					else
						TD_Length := Event.Tag_Directives'Length;
						TD_Body := new yaml_tag_directive_t_array (
							0 ..
							TD_Length + 1); -- extra +1
						for I in 0 .. TD_Length - 1 loop
							declare
								Item : Tag_Directive renames Event.Tag_Directives (
									Event.Tag_Directives'First + Integer (I));
							begin
								TD_Body (I).handle := strdup (Item.Handle);
								TD_Body (I).prefix := strdup (Item.Prefix);
							end;
						end loop;
						Tag_Directives_Start := TD_Body (0)'Unchecked_Access;
						Tag_Directives_End := TD_Body (TD_Length)'Unchecked_Access;
					end if;
					Error := C.yaml.yaml_document_start_event_initialize (
						Ev'Access,
						Version_Directive,
						Tag_Directives_Start,
						Tag_Directives_End,
						Boolean'Pos (Event.Implicit_Indicator)) = 0;
					if Tag_Directives_Start /= null then
						for I in 0 .. TD_Length - 1 loop
							C.stdlib.free (C.void_ptr (TD_Body (I).handle.all'Address));
							C.stdlib.free (C.void_ptr (TD_Body (I).prefix.all'Address));
						end loop;
						Free (TD_Body);
					end if;
					if Error then
						raise Storage_Error; -- maybe ...
					end if;
				end;
			when Document_End =>
				if C.yaml.yaml_document_end_event_initialize (
					Ev'Access,
					Boolean'Pos (Event.Implicit_Indicator)) = 0
				then
					raise Storage_Error; -- maybe ...
				end if;
			when Alias =>
				declare
					Anchor : C.yaml.yaml_char_t_ptr;
					Error : Boolean;
				begin
					Anchor := strdup (Event.Anchor);
					Error := C.yaml.yaml_alias_event_initialize (Ev'Access, Anchor) = 0;
					if Anchor /= null then
						C.stdlib.free (C.void_ptr (Anchor.all'Address));
					end if;
					if Error then
						raise Storage_Error; -- maybe ...
					end if;
				end;
			when Scalar =>
				declare
					Anchor : C.yaml.yaml_char_t_ptr;
					Tag : C.yaml.yaml_char_t_ptr;
					Ada_Value : String renames Event.Value.all;
					C_Value : array (0 .. Ada_Value'Length - 1) of aliased C.yaml.yaml_char_t;
					for C_Value'Address use Ada_Value'Address;
					pragma Suppress (Index_Check, C_Value);
					Error : Boolean;
				begin
					Anchor := strdup (Event.Anchor);
					Tag := strdup (Event.Tag);
					Error := C.yaml.yaml_scalar_event_initialize (
						Ev'Access,
						Anchor,
						Tag,
						C_Value (C_Value'First)'Access,
						C_Value'Length,
						Boolean'Pos (Event.Plain_Implicit_Tag),
						Boolean'Pos (Event.Quoted_Implicit_Tag),
						C.yaml.yaml_scalar_style_t'Enum_Val (
							Scalar_Style'Enum_Rep (Event.Scalar_Style))) = 0;
					if Anchor /= null then
						C.stdlib.free (C.void_ptr (Anchor.all'Address));
					end if;
					if Tag /= null then
						C.stdlib.free (C.void_ptr (Tag.all'Address));
					end if;
					if Error then
						raise Storage_Error; -- maybe ...
					end if;
				end;
			when Sequence_Start =>
				declare
					Anchor : C.yaml.yaml_char_t_ptr;
					Tag : C.yaml.yaml_char_t_ptr;
					Error : Boolean;
				begin
					Anchor := strdup (Event.Anchor);
					Tag := strdup (Event.Tag);
					Error := C.yaml.yaml_sequence_start_event_initialize (
						Ev'Access,
						Anchor,
						Tag,
						Boolean'Pos (Event.Implicit_Tag),
						C.yaml.yaml_sequence_style_t'Enum_Val (
							Sequence_Style'Enum_Rep (Event.Sequence_Style))) = 0;
					if Anchor /= null then
						C.stdlib.free (C.void_ptr (Anchor.all'Address));
					end if;
					if Tag /= null then
						C.stdlib.free (C.void_ptr (Tag.all'Address));
					end if;
					if Error then
						raise Storage_Error; -- maybe ...
					end if;
				end;
			when Sequence_End =>
				if C.yaml.yaml_sequence_end_event_initialize (Ev'Access) = 0 then
					raise Storage_Error; -- maybe ...
				end if;
			when Mapping_Start =>
				declare
					Anchor : C.yaml.yaml_char_t_ptr;
					Tag : C.yaml.yaml_char_t_ptr;
					Error : Boolean;
				begin
					Anchor := strdup (Event.Anchor);
					Tag := strdup (Event.Tag);
					Error := C.yaml.yaml_mapping_start_event_initialize (
						Ev'Access,
						Anchor,
						Tag,
						Boolean'Pos (Event.Implicit_Tag),
						C.yaml.yaml_mapping_style_t'Enum_Val (
							Mapping_Style'Enum_Rep (Event.Mapping_Style))) = 0;
					if Anchor /= null then
						C.stdlib.free (C.void_ptr (Anchor.all'Address));
					end if;
					if Tag /= null then
						C.stdlib.free (C.void_ptr (Tag.all'Address));
					end if;
					if Error then
						raise Storage_Error; -- maybe ...
					end if;
				end;
			when Mapping_End =>
				if C.yaml.yaml_mapping_end_event_initialize (Ev'Access) = 0 then
					raise Storage_Error; -- maybe ...
				end if;
		end case;
		if C.yaml.yaml_emitter_emit (Object.Raw'Access, Ev'Access) = 0 then
			Raise_Error (Object.Raw.error, Object.Raw.problem, null);
		end if;
	end Emit;
	
	overriding procedure Finalize (Object : in out Parser) is
	begin
		C.yaml.yaml_parser_delete (Object.Raw'Access);
	end Finalize;
	
	overriding procedure Finalize (Object : in out Emitter) is
	begin
		C.yaml.yaml_emitter_delete (Object.Raw'Access);
	end Finalize;
	
	procedure Flush (Object : in out Emitter) is
	begin
		if C.yaml.yaml_emitter_flush (Object.Raw'Access) = 0 then
			Raise_Error (Object.Raw.error, Object.Raw.problem, null);
		end if;
	end Flush;
	
	procedure Parse (
		Object : in out Parser;
		Process : not null access procedure (
			Event : in YAML.Event;
			Start_Mark, End_Mark : in Mark))
	is
		Ev : aliased C.yaml.yaml_event_t;
		Start_Mark, End_Mark : Mark;
	begin
		if C.yaml.yaml_parser_parse (Object.Raw'Access, Ev'Access) = 0 then
			Raise_Error (Object.Raw.error, Object.Raw.problem, Object.Raw.mark'Access);
		end if;
		Start_Mark.Index := Integer (Ev.start_mark.index);
		Start_Mark.Line := Integer (Ev.start_mark.line);
		Start_Mark.Column := Integer (Ev.start_mark.column);
		End_Mark.Index := Integer (Ev.end_mark.index);
		End_Mark.Line := Integer (Ev.end_mark.line);
		End_Mark.Column := Integer (Ev.end_mark.column);
		case Ev.F_type is
			when C.yaml.YAML_NO_EVENT =>
				Process (
					Event'(
						Event_Type => No_Event),
					Start_Mark,
					End_Mark);
			when C.yaml.YAML_STREAM_START_EVENT =>
				Process (
					Event'(
						Event_Type => Stream_Start,
						Encoding => Encoding'Enum_Val (C.yaml.yaml_encoding_t'Enum_Rep (
							Ev.data.stream_start.encoding))),
					Start_Mark,
					End_Mark);
			when C.yaml.YAML_STREAM_END_EVENT =>
				Process (
					Event'(
						Event_Type => Stream_End),
					Start_Mark,
					End_Mark);
			when C.yaml.YAML_DOCUMENT_START_EVENT =>
				declare
					Version_Directive : Version_Directive_Access;
					VD_Body : aliased YAML.Version_Directive;
					Tag_Directives : Tag_Directive_Array_Access;
				begin
					if Ev.data.document_start.version_directive = null then
						Version_Directive := null;
					else
						VD_Body.Major := Integer (
							Ev.data.document_start.version_directive.major);
						VD_Body.Minor := Integer (
							Ev.data.document_start.version_directive.minor);
						Version_Directive := VD_Body'Unchecked_Access;
					end if;
					if Ev.data.document_start.tag_directives.start /= null then
						declare
							function Cast is new Ada.Unchecked_Conversion (
								C.yaml.yaml_tag_directive_t_ptr,
								C.ptrdiff_t);
							function Cast is new Ada.Unchecked_Conversion (
								C.ptrdiff_t,
								C.yaml.yaml_tag_directive_t_ptr);
							sizeof_yaml_tag_directive_t : constant C.ptrdiff_t :=
								C.yaml.yaml_tag_directive_t'Size / Standard'Storage_Unit;
							Length : constant Natural := Natural (
								(Cast (Ev.data.document_start.tag_directives.F_end)
									- Cast (Ev.data.document_start.tag_directives.start))
								/ sizeof_yaml_tag_directive_t);
							P : C.yaml.yaml_tag_directive_t_ptr;
						begin
							Tag_Directives := new Tag_Directive_Array (1 .. Length);
							P := Ev.data.document_start.tag_directives.start;
							for I in 1 .. Length loop
								Tag_Directives (I).Handle := New_String (P.handle);
								Tag_Directives (I).Prefix := New_String (P.prefix);
								P := Cast (Cast (P) + sizeof_yaml_tag_directive_t);
							end loop;
						end;
					end if;
					Process (
						Event'(
							Event_Type => Document_Start,
							Version_Directive => Version_Directive,
							Tag_Directives => Tag_Directives,
							Implicit_Indicator => Ev.data.document_start.implicit /= 0),
						Start_Mark,
						End_Mark);
					if Tag_Directives /= null then
						for I in Tag_Directives'Range loop
							declare
								type SCA is access constant String;
								function Cast is new Ada.Unchecked_Conversion (
									SCA,
									String_Access);
								S : String_Access;
							begin
								S := Cast (SCA (Tag_Directives (I).Prefix));
								Free (S);
								S := Cast (SCA (Tag_Directives (I).Handle));
								Free (S);
							end;
						end loop;
						Free (Tag_Directives);
					end if;
				end;
			when C.yaml.YAML_DOCUMENT_END_EVENT =>
				Process (
					Event'(
						Event_Type => Document_End,
						Implicit_Indicator => Ev.data.document_end.implicit /= 0),
					Start_Mark,
					End_Mark);
			when C.yaml.YAML_ALIAS_EVENT =>
				declare
					Anchor : String_Access;
				begin
					Anchor := New_String (Ev.data.alias.anchor);
					Process (
						Event'(
							Event_Type => Alias,
							Anchor => Anchor),
						Start_Mark,
						End_Mark);
					Free (Anchor);
				end;
			when C.yaml.YAML_SCALAR_EVENT =>
				declare
					Anchor : String_Access;
					Tag : String_Access;
					Value : String (1 .. Natural (Ev.data.scalar.length));
					for Value'Address use Ev.data.scalar.value.all'Address;
				begin
					Anchor := New_String (Ev.data.scalar.anchor);
					Tag := New_String (Ev.data.scalar.tag);
					Process (
						Event'(
							Event_Type => Scalar,
							Anchor => Anchor,
							Tag => Tag,
							Value => Value'Unrestricted_Access,
							Plain_Implicit_Tag => Ev.data.scalar.plain_implicit /= 0,
							Quoted_Implicit_Tag => Ev.data.scalar.quoted_implicit /= 0,
							Scalar_Style => Scalar_Style'Enum_Val (
								C.yaml.yaml_scalar_style_t'Enum_Rep (Ev.data.scalar.style))),
						Start_Mark,
						End_Mark);
					Free (Anchor);
					Free (Tag);
				end;
			when C.yaml.YAML_SEQUENCE_START_EVENT =>
				declare
					Anchor : String_Access;
					Tag : String_Access;
				begin
					Anchor := New_String (Ev.data.sequence_start.anchor);
					Tag := New_String (Ev.data.sequence_start.tag);
					Process (
						Event'(
							Event_Type => Sequence_Start,
							Anchor => Anchor,
							Tag => Tag,
							Implicit_Tag => Ev.data.sequence_start.implicit /= 0,
							Sequence_Style => Sequence_Style'Enum_Val (
								C.yaml.yaml_sequence_style_t'Enum_Rep (Ev.data.sequence_start.style))),
						Start_Mark,
						End_Mark);
					Free (Anchor);
					Free (Tag);
				end;
			when C.yaml.YAML_SEQUENCE_END_EVENT =>
				Process (
					Event'(
						Event_Type => Sequence_End),
					Start_Mark,
					End_Mark);
			when C.yaml.YAML_MAPPING_START_EVENT =>
				declare
					Anchor : String_Access;
					Tag : String_Access;
				begin
					Anchor := New_String (Ev.data.mapping_start.anchor);
					Tag := New_String (Ev.data.mapping_start.tag);
					Process (
						Event'(
							Event_Type => Mapping_Start,
							Anchor => Anchor,
							Tag => Tag,
							Implicit_Tag => Ev.data.mapping_start.implicit /= 0,
							Mapping_Style => Mapping_Style'Enum_Val (
								C.yaml.yaml_mapping_style_t'Enum_Rep (Ev.data.mapping_start.style))),
						Start_Mark,
						End_Mark);
					Free (Anchor);
					Free (Tag);
				end;
			when C.yaml.YAML_MAPPING_END_EVENT =>
				Process (
					Event'(
						Event_Type => Mapping_End),
					Start_Mark,
					End_Mark);
		end case;
	end Parse;
	
	procedure Parse_Document_Start (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_DOCUMENT_START_EVENT);
	end Parse_Document_Start;
	
	procedure Parse_Document_End (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_DOCUMENT_END_EVENT);
	end Parse_Document_End;
	
	procedure Parse_Stream_Start (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_STREAM_START_EVENT);
	end Parse_Stream_Start;
	
	procedure Parse_Stream_End (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_STREAM_END_EVENT);
	end Parse_Stream_End;
	
	procedure Raise_Error (
		Error : in C.yaml.yaml_error_type_t;
		Problem : access constant C.char;
		Mark : access constant C.yaml.yaml_mark_t)
	is
		function Image (Mark : access constant C.yaml.yaml_mark_t) return String is
		begin
			if Mark = null then
				return "";
			else
				return "line" & C.size_t'Image (Mark.line) & ": ";
			end if;
		end Image;
		Message : constant String := Image (Mark) & To_String (Problem);
	begin
		case Error is
			when C.yaml.YAML_MEMORY_ERROR =>
				raise Storage_Error with Message;
			when C.yaml.YAML_SCANNER_ERROR | C.yaml.YAML_PARSER_ERROR =>
				raise Data_Error with Message;
			when others =>
				null; -- move to below to suppress the warning, bug(?) of gcc-4.5.1
		end case;
		raise Status_Error with Message;
	end Raise_Error;
	
	procedure Set_Break (
		Object : in out Emitter;
		Break : in Line_Break) is
	begin
		C.yaml.yaml_emitter_set_break (
			Object.Raw'Access,
			C.yaml.yaml_break_t'Enum_Val (Line_Break'Enum_Rep (Break)));
	end Set_Break;
	
	procedure Set_Canonical (
		Object : in out Emitter;
		Canonical : in Boolean) is
	begin
		C.yaml.yaml_emitter_set_canonical (
			Object.Raw'Access,
			Boolean'Pos (Canonical));
	end Set_Canonical;
	
	procedure Set_Encoding (
		Object : in out Parser;
		Encoding : in YAML.Encoding) is
	begin
		C.yaml.yaml_parser_set_encoding (
			Object.Raw'Access,
			C.yaml.yaml_encoding_t'Enum_Val (YAML.Encoding'Enum_Rep (Encoding)));
	end Set_Encoding;
	
	procedure Set_Encoding (
		Object : in out Emitter;
		Encoding : in YAML.Encoding) is
	begin
		C.yaml.yaml_emitter_set_encoding (
			Object.Raw'Access,
			C.yaml.yaml_encoding_t'Enum_Val (YAML.Encoding'Enum_Rep (Encoding)));
	end Set_Encoding;
	
	procedure Set_Indent (
		Object : in out Emitter;
		Indent : in Indent_Width) is
	begin
		C.yaml.yaml_emitter_set_indent (
			Object.Raw'Access,
			C.signed_int (Indent));
	end Set_Indent;
	
	procedure Set_Unicode (
		Object : in out Emitter;
		Unicode : in Boolean) is
	begin
		C.yaml.yaml_emitter_set_unicode (
			Object.Raw'Access,
			Boolean'Pos (Unicode));
	end Set_Unicode;
	
	procedure Set_Width (
		Object : in out Emitter;
		Width : in Line_Width) is
	begin
		C.yaml.yaml_emitter_set_width (
			Object.Raw'Access,
			C.signed_int (Width));
	end Set_Width;
	
	function Version return String is
	begin
		return To_String (C.yaml.yaml_get_version_string);
	end Version;
	
end YAML;
