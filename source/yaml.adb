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
	use type C.yaml.yaml_emitter_state_t;
	use type C.yaml.yaml_event_type_t;
	use type C.yaml.yaml_parser_state_t;
	use type C.yaml.yaml_tag_directive_t_ptr;
	use type C.yaml.yaml_version_directive_t_ptr;
	
	function strdup (S : access constant String) return C.yaml.yaml_char_t_ptr is
	begin
		if S = null then
			return null;
		else
			declare
				function Cast is
					new Ada.Unchecked_Conversion (C.void_ptr, C.yaml.yaml_char_t_ptr);
				function Cast is
					new Ada.Unchecked_Conversion (C.yaml.yaml_char_t_ptr, C.ptrdiff_t);
				function Cast is
					new Ada.Unchecked_Conversion (C.ptrdiff_t, C.yaml.yaml_char_t_ptr);
				Length : constant C.size_t := S'Length;
				p : constant C.void_ptr := C.stdlib.malloc (Length + 1);
			begin
				declare
					Dest : String (S'Range);
					for Dest'Address use System.Address (p);
				begin
					Dest := S.all;
				end;
				Cast (Cast (Cast (p)) + C.ptrdiff_t (Length)).all :=
					C.yaml.yaml_char_t'Val (0);
				return Cast (p);
			end;
		end if;
	end strdup;
	
	type String_Access is access String;
	procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
	
	function To_char_const_ptr is
		new Ada.Unchecked_Conversion (C.yaml.yaml_char_t_ptr, C.char_const_ptr);
	
	function To_Address is
		new Ada.Unchecked_Conversion (C.char_const_ptr, System.Address);
	
	function Length (S : access constant C.char) return Natural is
	begin
		if S = null then
			return 0;
		else
			return Natural (C.string.strlen (S));
		end if;
	end Length;
	
	function To_String (S : access constant C.char) return String is
		Result : String (1 .. Length (S));
		for Result'Address use To_Address (C.char_const_ptr (S));
	begin
		return Result;
	end To_String;
	
	function New_String (S : C.yaml.yaml_char_t_ptr) return String_Access is
	begin
		if S = null then
			return null;
		else
			declare
				Length : constant Natural := Natural (C.string.strlen (To_char_const_ptr (S)));
			begin
				return Result : constant String_Access := new String (1 .. Length) do
					declare
						Source : String (1 .. Length);
						for Source'Address use S.all'Address;
					begin
						Result.all := Source;
					end;
				end return;
			end;
		end if;
	end New_String;
	
	-- dirty trick
	function Copy_String_Access (
		S : not null String_Access;
		Constraint : not null access String_Constraint)
		return String_Access
	is
		type Fat_Type is record
			Data : System.Address;
			Constraints : System.Address;
		end record;
		Fat : Fat_Type;
		Result : aliased String_Access;
		for Fat'Address use Result'Address;
	begin
		Fat.Data := S.all'Address;
		Fat.Constraints := Constraint.all'Address;
		Constraint.First := S'First;
		Constraint.Last := S'Last;
		return Result;
	end Copy_String_Access;
	
	type String_Constant_Access is access constant String;
	function Remove_Constant is
		new Ada.Unchecked_Conversion (String_Constant_Access, String_Access);
	
	-- implementation
	
	function Version return String is
		P : constant C.char_const_ptr := C.yaml.yaml_get_version_string;
	begin
		return To_String (P);
	end Version;
	
	-- parser
	
	type Version_Directive_Access is access all Version_Directive;
	
	type Tag_Directive_Array_Access is access Tag_Directive_Array;
	procedure Free is
		new Ada.Unchecked_Deallocation (
			Tag_Directive_Array,
			Tag_Directive_Array_Access);
	
	type Tag_Directive_Array_Constant_Access is
		access constant Tag_Directive_Array;
	function Remove_Constant is
		new Ada.Unchecked_Conversion (
			Tag_Directive_Array_Constant_Access,
			Tag_Directive_Array_Access);
	
	function Read_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t;
		size_read : access C.size_t)
		return C.signed_int
		with Convention => C;
	
	function Read_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t;
		size_read : access C.size_t)
		return C.signed_int
	is
		type I is access procedure (Item : out String; Last : out Natural);
		function To_Input is new Ada.Unchecked_Conversion (C.void_ptr, I);
		Ada_Data : String (1 .. Natural (size));
		for Ada_Data'Address use buffer.all'Address;
		Last : Natural;
	begin
		To_Input (data) (Ada_Data, Last);
		size_read.all := C.size_t (Last);
		return 1;
	end Read_Handler;
	
	procedure Delete_Event (Parsed_Data : in out Parsed_Data_Type) is
	begin
		C.yaml.yaml_event_delete (Parsed_Data.U.yaml_event'Access);
	end Delete_Event;
	
	procedure Delete_Document_Start_Event (
		Parsed_Data : in out Parsed_Data_Type) is
	begin
		declare
			Tag_Directives : Tag_Directive_Array_Access :=
				Remove_Constant (
					Tag_Directive_Array_Constant_Access (Parsed_Data.U.Event.Tag_Directives));
		begin
			if Tag_Directives /= null then
				for I in Tag_Directives'Range loop
					declare
						S : String_Access;
					begin
						S := Remove_Constant (String_Constant_Access (Tag_Directives (I).Prefix));
						Free (S);
						S := Remove_Constant (String_Constant_Access (Tag_Directives (I).Handle));
						Free (S);
					end;
				end loop;
				Free (Tag_Directives);
			end if;
		end;
		C.yaml.yaml_event_delete (Parsed_Data.U.yaml_event'Access);
	end Delete_Document_Start_Event;
	
	procedure Parse (
		Raw_Object : not null access C.yaml.yaml_parser_t;
		Parsed_Data : out Parsed_Data_Type)
	is
		Ev : C.yaml.yaml_event_t renames Parsed_Data.U.yaml_event;
	begin
		if C.yaml.yaml_parser_parse (Raw_Object, Ev'Access) = 0 then
			Raise_Error (Raw_Object.error, Raw_Object.problem, Raw_Object.mark'Access);
		end if;
		Parsed_Data.U.Start_Mark.Index := Integer (Ev.start_mark.index);
		Parsed_Data.U.Start_Mark.Line := Integer (Ev.start_mark.line);
		Parsed_Data.U.Start_Mark.Column := Integer (Ev.start_mark.column);
		Parsed_Data.U.End_Mark.Index := Integer (Ev.end_mark.index);
		Parsed_Data.U.End_Mark.Line := Integer (Ev.end_mark.line);
		Parsed_Data.U.End_Mark.Column := Integer (Ev.end_mark.column);
		case Ev.F_type is
			when C.yaml.YAML_NO_EVENT =>
				Parsed_Data.U.Event := Event'(Event_Type => No_Event);
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_STREAM_START_EVENT =>
				Parsed_Data.U.Event :=
					Event'(
						Event_Type => Stream_Start,
						Encoding =>
							Encoding'Enum_Val (
								C.yaml.yaml_encoding_t'Enum_Rep (Ev.data.stream_start.encoding)));
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_STREAM_END_EVENT =>
				Parsed_Data.U.Event := Event'(Event_Type => Stream_End);
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_DOCUMENT_START_EVENT =>
				declare
					Version_Directive : Version_Directive_Access;
				begin
					if Ev.data.document_start.version_directive = null then
						Version_Directive := null;
					else
						Version_Directive := Parsed_Data.U.Version_Directive'Unchecked_Access;
						Version_Directive.Major :=
							Integer (Ev.data.document_start.version_directive.major);
						Version_Directive.Minor :=
							Integer (Ev.data.document_start.version_directive.minor);
					end if;
					Parsed_Data.U.Event :=
						Event'(
							Event_Type => Document_Start,
							Version_Directive => Version_Directive,
							Tag_Directives => null,
							Implicit_Indicator => Ev.data.document_start.implicit /= 0);
				end;
				Parsed_Data.Delete := Delete_Document_Start_Event'Access;
				-- allocating Tag_Directives
				if Ev.data.document_start.tag_directives.start /= null then
					declare
						function Cast is
							new Ada.Unchecked_Conversion (C.yaml.yaml_tag_directive_t_ptr, C.ptrdiff_t);
						function Cast is
							new Ada.Unchecked_Conversion (C.ptrdiff_t, C.yaml.yaml_tag_directive_t_ptr);
						sizeof_yaml_tag_directive_t : constant C.ptrdiff_t :=
							C.yaml.yaml_tag_directive_t'Size / Standard'Storage_Unit;
						Length : constant Natural :=
							Natural (
								(Cast (Ev.data.document_start.tag_directives.F_end)
										- Cast (Ev.data.document_start.tag_directives.start))
									/ sizeof_yaml_tag_directive_t);
						Tag_Directives : Tag_Directive_Array_Access;
						P : C.yaml.yaml_tag_directive_t_ptr;
					begin
						Tag_Directives := new Tag_Directive_Array (1 .. Length);
						Parsed_Data.U.Event.Tag_Directives := Tag_Directives; -- hold into RAII
						P := Ev.data.document_start.tag_directives.start;
						for I in 1 .. Length loop
							Tag_Directives (I).Handle := New_String (P.handle);
							Tag_Directives (I).Prefix := New_String (P.prefix);
							P := Cast (Cast (P) + sizeof_yaml_tag_directive_t);
						end loop;
					end;
				end if;
			when C.yaml.YAML_DOCUMENT_END_EVENT =>
				Parsed_Data.U.Event :=
					Event'(
						Event_Type => Document_End,
						Implicit_Indicator => Ev.data.document_end.implicit /= 0);
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_ALIAS_EVENT =>
				declare
					-- anchor
					Anchor_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.alias.anchor)));
					for Anchor_S'Address use To_Address (To_char_const_ptr (Ev.data.alias.anchor));
					Anchor_A : constant String_Access :=
						Copy_String_Access (
							Anchor_S'Unrestricted_Access,
							Parsed_Data.U.Anchor_Constraint'Access);
				begin
					Parsed_Data.U.Event := Event'(Event_Type => Alias, Anchor => Anchor_A);
				end;
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_SCALAR_EVENT =>
				declare
					-- anchor
					Anchor_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.scalar.anchor)));
					for Anchor_S'Address use
						To_Address (To_char_const_ptr (Ev.data.scalar.anchor));
					Anchor_A : constant String_Access :=
						Copy_String_Access (
							Anchor_S'Unrestricted_Access,
							Parsed_Data.U.Anchor_Constraint'Access);
					-- tag
					Tag_S : aliased String (1 .. Length (To_char_const_ptr (Ev.data.scalar.tag)));
					for Tag_S'Address use To_Address (To_char_const_ptr (Ev.data.scalar.tag));
					Tag_A : constant String_Access :=
						Copy_String_Access (
							Tag_S'Unrestricted_Access,
							Parsed_Data.U.Tag_Constraint'Access);
					-- value
					Value_S : aliased String (1 .. Natural (Ev.data.scalar.length));
					for Value_S'Address use To_Address (To_char_const_ptr (Ev.data.scalar.value));
					Value_A : constant String_Access :=
						Copy_String_Access (
							Value_S'Unrestricted_Access,
							Parsed_Data.U.Value_Constraint'Access);
				begin
					Parsed_Data.U.Event :=
						Event'(
							Event_Type => Scalar,
							Anchor => Anchor_A,
							Tag => Tag_A,
							Value => Value_A,
							Plain_Implicit_Tag => Ev.data.scalar.plain_implicit /= 0,
							Quoted_Implicit_Tag => Ev.data.scalar.quoted_implicit /= 0,
							Scalar_Style =>
								Scalar_Style'Enum_Val (
									C.yaml.yaml_scalar_style_t'Enum_Rep (Ev.data.scalar.style)));
				end;
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_SEQUENCE_START_EVENT =>
				declare
					-- anchor
					Anchor_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.sequence_start.anchor)));
					for Anchor_S'Address use
						To_Address (To_char_const_ptr (Ev.data.sequence_start.anchor));
					Anchor_A : constant String_Access :=
						Copy_String_Access (
							Anchor_S'Unrestricted_Access,
							Parsed_Data.U.Anchor_Constraint'Access);
					-- tag
					Tag_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.sequence_start.tag)));
					for Tag_S'Address use
						To_Address (To_char_const_ptr (Ev.data.sequence_start.tag));
					Tag_A : constant String_Access :=
						Copy_String_Access (
							Tag_S'Unrestricted_Access,
							Parsed_Data.U.Tag_Constraint'Access);
				begin
					Parsed_Data.U.Event :=
						Event'(
							Event_Type => Sequence_Start,
							Anchor => Anchor_A,
							Tag => Tag_A,
							Implicit_Tag => Ev.data.sequence_start.implicit /= 0,
							Sequence_Style =>
								Sequence_Style'Enum_Val (
									C.yaml.yaml_sequence_style_t'Enum_Rep (Ev.data.sequence_start.style)));
				end;
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_SEQUENCE_END_EVENT =>
				Parsed_Data.U.Event := Event'(Event_Type => Sequence_End);
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_MAPPING_START_EVENT =>
				declare
					-- anchor
					Anchor_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.mapping_start.anchor)));
					for Anchor_S'Address use
						To_Address (To_char_const_ptr (Ev.data.mapping_start.anchor));
					Anchor_A : constant String_Access :=
						Copy_String_Access (
							Anchor_S'Unrestricted_Access,
							Parsed_Data.U.Anchor_Constraint'Access);
					-- tag
					Tag_S : aliased
						String (1 .. Length (To_char_const_ptr (Ev.data.mapping_start.tag)));
					for Tag_S'Address use
						To_Address (To_char_const_ptr (Ev.data.mapping_start.tag));
					Tag_A : constant String_Access :=
						Copy_String_Access (
							Tag_S'Unrestricted_Access,
							Parsed_Data.U.Tag_Constraint'Access);
				begin
					Parsed_Data.U.Event :=
						Event'(
							Event_Type => Mapping_Start,
							Anchor => Anchor_A,
							Tag => Tag_A,
							Implicit_Tag => Ev.data.mapping_start.implicit /= 0,
							Mapping_Style =>
								Mapping_Style'Enum_Val (
									C.yaml.yaml_mapping_style_t'Enum_Rep (Ev.data.mapping_start.style)));
				end;
				Parsed_Data.Delete := Delete_Event'Access;
			when C.yaml.YAML_MAPPING_END_EVENT =>
				Parsed_Data.U.Event := Event'(Event_Type => Mapping_End);
				Parsed_Data.Delete := Delete_Event'Access;
		end case;
	end Parse;
	
	procedure Parse_Expection (
		Raw_Object : not null access C.yaml.yaml_parser_t;
		Expected : C.yaml.yaml_event_type_t)
	is
		Ev : aliased C.yaml.yaml_event_t;
		pragma Suppress_Initialization (Ev);
		T : C.yaml.yaml_event_type_t;
	begin
		if C.yaml.yaml_parser_parse (Raw_Object, Ev'Access) = 0 then
			Raise_Error (Raw_Object.error, Raw_Object.problem, Raw_Object.mark'Access);
		end if;
		T := Ev.F_type;
		C.yaml.yaml_event_delete (Ev'Access);
		if T /= Expected then
			raise Data_Error;
		end if;
	end Parse_Expection;
	
	-- parser: wrappers
	
	procedure Parse (Object : in out Parser; Parsed_Data : out Parsed_Data_Type) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_parser_t) is
		begin
			Parse (Raw_Object, Parsed_Data);
		end Process;
		procedure Do_Parse is new Controlled_Parsers.Update (Process);
	begin
		Do_Parse (Object);
	end Parse;
	
	procedure Parse_Expection (
		Object : in out Parser;
		Expected : C.yaml.yaml_event_type_t)
	is
		procedure Process (Raw_Object : not null access C.yaml.yaml_parser_t) is
		begin
			Parse_Expection (Raw_Object, Expected);
		end Process;
		procedure Do_Parse_Expection is new Controlled_Parsers.Update (Process);
	begin
		Do_Parse_Expection (Object);
	end Parse_Expection;
	
	-- implementation of parser
	
	function Is_Assigned (Parsing_Entry : Parsing_Entry_Type) return Boolean is
		function Process (Raw_Data : Parsed_Data_Type) return Boolean is
		begin
			return Raw_Data.Delete /= null;
		end Process;
		function Do_Is_Assigned is
			new Controlled_Parsing_Entries.Query (Boolean, Process);
	begin
		return Do_Is_Assigned (Parsing_Entry);
	end Is_Assigned;
	
	function Value (Parsing_Entry : aliased Parsing_Entry_Type)
		return Event_Reference_Type
	is
		pragma Check (Pre,
			Check => Is_Assigned (Parsing_Entry) or else raise Status_Error);
	begin
		return (Element =>
			Controlled_Parsing_Entries.Constant_Reference (Parsing_Entry).U.Event'Access);
	end Value;
	
	function Start_Mark (Parsing_Entry : aliased Parsing_Entry_Type)
		return Mark_Reference_Type
	is
		pragma Check (Pre,
			Check => Is_Assigned (Parsing_Entry) or else raise Status_Error);
	begin
		return (Element =>
			Controlled_Parsing_Entries.Constant_Reference (Parsing_Entry).U
				.Start_Mark'Access);
	end Start_Mark;
	
	function End_Mark (Parsing_Entry : aliased Parsing_Entry_Type)
		return Mark_Reference_Type
	is
		pragma Check (Pre,
			Check => Is_Assigned (Parsing_Entry) or else raise Status_Error);
	begin
		return (Element =>
			Controlled_Parsing_Entries.Constant_Reference (Parsing_Entry).U
				.End_Mark'Access);
	end End_Mark;
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural))
		return Parser
	is
		type I is access procedure (Item : out String; Last : out Natural);
		function To_void_ptr is new Ada.Unchecked_Conversion (I, C.void_ptr);
	begin
		return Result : Parser do
			declare
				procedure Process (Raw_Result : not null access C.yaml.yaml_parser_t) is
				begin
					if C.yaml.yaml_parser_initialize (Raw_Result) = 0 then
						Raise_Error (Raw_Result.error, Raw_Result.problem, null);
					end if;
					C.yaml.yaml_parser_set_input (
						Raw_Result,
						Read_Handler'Access,
						To_void_ptr (Input));
				end Process;
				procedure Do_Create is new Controlled_Parsers.Update (Process);
			begin
				Do_Create (Result);
			end;
		end return;
	end Create;
	
	procedure Set_Encoding (Object : in out Parser; Encoding : in YAML.Encoding) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_parser_t) is
		begin
			C.yaml.yaml_parser_set_encoding (
				Raw_Object,
				C.yaml.yaml_encoding_t'Enum_Val (YAML.Encoding'Enum_Rep (Encoding)));
		end Process;
		procedure Do_Set_Encoding is new Controlled_Parsers.Update (Process);
	begin
		Do_Set_Encoding (Object);
	end Set_Encoding;
	
	procedure Get (
		Object : in out Parser;
		Process : not null access procedure (
			Event : in YAML.Event;
			Start_Mark, End_Mark : in Mark))
	is
		Parsing_Entry : aliased Parsing_Entry_Type;
	begin
		Get (Object, Parsing_Entry);
		Process (
			Value (Parsing_Entry).Element.all,
			Start_Mark => Start_Mark (Parsing_Entry).Element.all,
			End_Mark => End_Mark (Parsing_Entry).Element.all);
	end Get;
	
	procedure Get (
		Object : in out Parser;
		Parsing_Entry : out Parsing_Entry_Type)
	is
		procedure Process (Raw_Data : in out Parsed_Data_Type) is
		begin
			if Raw_Data.Delete /= null then
				Raw_Data.Delete (Raw_Data);
				Raw_Data.Delete := null;
			end if;
			Parse (Object, Raw_Data);
		end Process;
		procedure Do_Get is new Controlled_Parsing_Entries.Update (Process);
	begin
		Do_Get (Parsing_Entry);
	end Get;
	
	procedure Get_Document_Start (Object : in out Parser) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_parser_t) is
		begin
			if Raw_Object.state = C.yaml.YAML_PARSE_STREAM_START_STATE then
				Parse_Expection (Raw_Object, C.yaml.YAML_STREAM_START_EVENT);
			end if;
			Parse_Expection (Raw_Object, C.yaml.YAML_DOCUMENT_START_EVENT);
		end Process;
		procedure Do_Get_Document_Start is new Controlled_Parsers.Update (Process);
	begin
		Do_Get_Document_Start (Object);
	end Get_Document_Start;
	
	procedure Get_Document_End (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_DOCUMENT_END_EVENT);
	end Get_Document_End;
	
	procedure Finish (Object : in out Parser) is
	begin
		Parse_Expection (Object, C.yaml.YAML_STREAM_END_EVENT);
	end Finish;
	
	function Last_Error_Mark (Object : Parser) return Mark is
		function Process (Raw_Object : not null access constant C.yaml.yaml_parser_t)
			return Mark is
		begin
			return (Index => Integer (Raw_Object.problem_mark.index),
				Line => Integer (Raw_Object.problem_mark.line),
				Column => Integer (Raw_Object.problem_mark.column));
		end Process;
		function Do_Last_Error_Mark is new Controlled_Parsers.Query (Mark, Process);
	begin
		return Do_Last_Error_Mark (Object);
	end Last_Error_Mark;
	
	function Last_Error_Message (Object : Parser) return String is
		function Process (Raw_Object : not null access constant C.yaml.yaml_parser_t)
			return String is
		begin
			return To_String (Raw_Object.problem);
		end Process;
		function Do_Last_Error_Message is
			new Controlled_Parsers.Query (String, Process);
	begin
		return Do_Last_Error_Message (Object);
	end Last_Error_Message;
	
	-- private implementation of parser
	
	package body Controlled_Parsing_Entries is
		
		function Constant_Reference (Object : aliased Parsing_Entry_Type)
			return not null access constant Parsed_Data_Type;
		pragma Inline (Constant_Reference);
		
		function Constant_Reference (Object : aliased Parsing_Entry_Type)
			return not null access constant Parsed_Data_Type is
		begin
			return Object.Data'Access;
		end Constant_Reference;
		
		-- implementation
		
		function Constant_Reference (Object : aliased YAML.Parsing_Entry_Type)
			return not null access constant Parsed_Data_Type is
		begin
			return Constant_Reference (Parsing_Entry_Type (Object));
		end Constant_Reference;
		
		function Query (Object : YAML.Parsing_Entry_Type) return Result_Type is
			function Query (Object : Parsing_Entry_Type) return Result_Type;
			pragma Inline (Query);
			
			function Query (Object : Parsing_Entry_Type) return Result_Type is
			begin
				return Process (Object.Data);
			end Query;
		begin
			return Query (Parsing_Entry_Type (Object));
		end Query;
		
		procedure Update (Object : in out YAML.Parsing_Entry_Type) is
			procedure Update (Object : in out Parsing_Entry_Type);
			pragma Inline (Update);
			
			procedure Update (Object : in out Parsing_Entry_Type) is
			begin
				Process (Object.Data);
			end Update;
		begin
			Update (Parsing_Entry_Type (Object));
		end Update;
		
		overriding procedure Finalize (Object : in out Parsing_Entry_Type) is
		begin
			if Object.Data.Delete /= null then
				Object.Data.Delete (Object.Data);
			end if;
		end Finalize;
		
	end Controlled_Parsing_Entries;
	
	package body Controlled_Parsers is
		
		function Query (Object : YAML.Parser) return Result_Type is
			function Query (Object : Parser) return Result_Type;
			pragma Inline (Query);
			
			function Query (Object : Parser) return Result_Type is
			begin
				return Process (Object.Raw.X'Access);
			end Query;
		begin
			return Query (Parser (Object));
		end Query;
		
		procedure Update (Object : in out YAML.Parser) is
			procedure Update (Object : in out Parser);
			pragma Inline (Update);
			
			procedure Update (Object : in out Parser) is
			begin
				Process (Object.Raw.X'Access);
			end Update;
		begin
			Update (Parser (Object));
		end Update;
		
		overriding procedure Finalize (Object : in out Parser) is
		begin
			C.yaml.yaml_parser_delete (Object.Raw.X'Access);
		end Finalize;
		
	end Controlled_Parsers;
	
	-- emitter
	
	type yaml_tag_directive_t_array is
		array (C.size_t range <>) of aliased C.yaml.yaml_tag_directive_t;
	pragma Suppress_Initialization (yaml_tag_directive_t_array);
	type yaml_tag_directive_t_array_access is access yaml_tag_directive_t_array;
	procedure Free is
		new Ada.Unchecked_Deallocation (
			yaml_tag_directive_t_array,
			yaml_tag_directive_t_array_access);
	
	function Write_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t)
		return C.signed_int
		with Convention => C;
	
	function Write_Handler (
		data : C.void_ptr;
		buffer : access C.unsigned_char;
		size : C.size_t)
		return C.signed_int
	is
		type O is access procedure (Item : in String);
		function To_Output is new Ada.Unchecked_Conversion (C.void_ptr, O);
		Ada_Data : String (1 .. Natural (size));
		for Ada_Data'Address use buffer.all'Address;
	begin
		To_Output (data) (Ada_Data);
		return 1;
	end Write_Handler;
	
	procedure Emit (
		Raw_Object : not null access C.yaml.yaml_emitter_t;
		Event : in YAML.Event)
	is
		Ev : aliased C.yaml.yaml_event_t;
		pragma Suppress_Initialization (Ev);
	begin
		case Event.Event_Type is
			when No_Event =>
				raise Constraint_Error;
			when Stream_Start =>
				if C.yaml.yaml_stream_start_event_initialize (
					Ev'Access,
					C.yaml.yaml_encoding_t'Enum_Val (YAML.Encoding'Enum_Rep (Event.Encoding))) = 0
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
					if Event.Tag_Directives = null or else Event.Tag_Directives'Length = 0 then
						Tag_Directives_Start := null;
						Tag_Directives_End := null;
					else
						TD_Length := Event.Tag_Directives'Length;
						TD_Body := new yaml_tag_directive_t_array (0 .. TD_Length + 1); -- extra +1
						for I in 0 .. TD_Length - 1 loop
							declare
								Item : Tag_Directive
									renames Event.Tag_Directives (Event.Tag_Directives'First + Integer (I));
							begin
								TD_Body (I).handle := strdup (Item.Handle);
								TD_Body (I).prefix := strdup (Item.Prefix);
							end;
						end loop;
						Tag_Directives_Start := TD_Body (0)'Unchecked_Access;
						Tag_Directives_End := TD_Body (TD_Length)'Unchecked_Access;
					end if;
					Error :=
						C.yaml.yaml_document_start_event_initialize (
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
					Length : constant C.signed_int := Ada_Value'Length;
					C_Value :
						array (0 .. C.size_t (C.signed_int'Max (Length - 1, 0))) of aliased
							C.yaml.yaml_char_t;
					for C_Value'Address use Ada_Value'Address;
					Error : Boolean;
				begin
					Anchor := strdup (Event.Anchor);
					Tag := strdup (Event.Tag);
					Error :=
						C.yaml.yaml_scalar_event_initialize (
							Ev'Access,
							Anchor,
							Tag,
							C_Value (C_Value'First)'Access,
							Length,
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
					Error :=
						C.yaml.yaml_sequence_start_event_initialize (
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
					Error :=
						C.yaml.yaml_mapping_start_event_initialize (
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
		if C.yaml.yaml_emitter_emit (Raw_Object, Ev'Access) = 0 then
			Raise_Error (Raw_Object.error, Raw_Object.problem, null);
		end if;
	end Emit;
	
	-- implementation of emitter
	
	function Create (Output : not null access procedure (Item : in String))
		return Emitter
	is
		type O is access procedure (Item : in String);
		function To_void_ptr is new Ada.Unchecked_Conversion (O, C.void_ptr);
	begin
		return Result : Emitter do
			declare
				procedure Process (Raw_Result : not null access C.yaml.yaml_emitter_t) is
				begin
					if C.yaml.yaml_emitter_initialize (Raw_Result) = 0 then
						Raise_Error (Raw_Result.error, Raw_Result.problem, null);
					end if;
					C.yaml.yaml_emitter_set_output (
						Raw_Result,
						Write_Handler'Access,
						To_void_ptr (Output));
				end Process;
				procedure Do_Create is new Controlled_Emitters.Update (Process);
			begin
				Do_Create (Result);
			end;
		end return;
	end Create;
	
	procedure Flush (Object : in out Emitter) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			if C.yaml.yaml_emitter_flush (Raw_Object) = 0 then
				Raise_Error (Raw_Object.error, Raw_Object.problem, null);
			end if;
		end Process;
		procedure Do_Flush is new Controlled_Emitters.Update (Process);
	begin
		Do_Flush (Object);
	end Flush;
	
	procedure Set_Encoding (
		Object : in out Emitter;
		Encoding : in YAML.Encoding)
	is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_encoding (
				Raw_Object,
				C.yaml.yaml_encoding_t'Enum_Val (YAML.Encoding'Enum_Rep (Encoding)));
		end Process;
		procedure Do_Set_Encoding is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Encoding (Object);
	end Set_Encoding;
	
	procedure Set_Canonical (Object : in out Emitter; Canonical : in Boolean) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_canonical (Raw_Object, Boolean'Pos (Canonical));
		end Process;
		procedure Do_Set_Canonical is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Canonical (Object);
	end Set_Canonical;
	
	procedure Set_Indent (Object : in out Emitter; Indent : in Indent_Width) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_indent (Raw_Object, C.signed_int (Indent));
		end Process;
		procedure Do_Set_Indent is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Indent (Object);
	end Set_Indent;
	
	procedure Set_Width (Object : in out Emitter; Width : in Line_Width) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_width (Raw_Object, C.signed_int (Width));
		end Process;
		procedure Do_Set_Width is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Width (Object);
	end Set_Width;
	
	procedure Set_Unicode (Object : in out Emitter; Unicode : in Boolean) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_unicode (Raw_Object, Boolean'Pos (Unicode));
		end Process;
		procedure Do_Set_Unicode is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Unicode (Object);
	end Set_Unicode;
	
	procedure Set_Break (Object : in out Emitter; Break : in Line_Break) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			C.yaml.yaml_emitter_set_break (
				Raw_Object,
				C.yaml.yaml_break_t'Enum_Val (Line_Break'Enum_Rep (Break)));
		end Process;
		procedure Do_Set_Break is new Controlled_Emitters.Update (Process);
	begin
		Do_Set_Break (Object);
	end Set_Break;
	
	procedure Put (Object : in out Emitter; Event : in YAML.Event) is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			Emit (Raw_Object, Event);
		end Process;
		procedure Do_Put is new Controlled_Emitters.Update (Process);
	begin
		Do_Put (Object);
	end Put;
	
	procedure Put_Document_Start (
		Object : in out Emitter;
		Implicit_Indicator : in Boolean := False;
		Version_Directive : access constant YAML.Version_Directive := null;
		Tag_Directives : access constant YAML.Tag_Directive_Array := null)
	is
		procedure Process (Raw_Object : not null access C.yaml.yaml_emitter_t) is
		begin
			if Raw_Object.state = C.yaml.YAML_EMIT_STREAM_START_STATE then
				Emit (Raw_Object, (Event_Type => Stream_Start, Encoding => Any));
			end if;
			Emit (
				Raw_Object,
				(Event_Type => Document_Start,
					Implicit_Indicator => Implicit_Indicator,
					Version_Directive => Version_Directive,
					Tag_Directives => Tag_Directives));
		end Process;
		procedure Do_Put_Document_Start is new Controlled_Emitters.Update (Process);
	begin
		Do_Put_Document_Start (Object);
	end Put_Document_Start;
	
	procedure Put_Document_End (
		Object : in out Emitter;
		Implicit_Indicator : in Boolean := True) is
	begin
		Put (
			Object,
			(Event_Type => Document_End, Implicit_Indicator => Implicit_Indicator));
	end Put_Document_End;
	
	procedure Finish (Object : in out Emitter) is
	begin
		Put (Object, (Event_Type => Stream_End));
		Flush (Object);
	end Finish;
	
	-- private implementation of emitter
	
	package body Controlled_Emitters is
		
		procedure Update (Object : in out YAML.Emitter) is
			procedure Update (Object : in out Emitter);
			pragma Inline (Update);
			
			procedure Update (Object : in out Emitter) is
			begin
				Process (Object.Raw.X'Access);
			end Update;
		begin
			Update (Emitter (Object));
		end Update;
		
		overriding procedure Finalize (Object : in out Emitter) is
		begin
			C.yaml.yaml_emitter_delete (Object.Raw.X'Access);
		end Finalize;
		
	end Controlled_Emitters;
	
	-- private implementation of exceptions
	
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
		Ada_Problem : String (1 .. Length (Problem));
		for Ada_Problem'Address use To_Address (C.char_const_ptr (Problem));
		Message : constant String := Image (Mark) & Ada_Problem;
	begin
		case Error is
			when C.yaml.YAML_MEMORY_ERROR =>
				raise Storage_Error with Message;
			when C.yaml.YAML_SCANNER_ERROR | C.yaml.YAML_PARSER_ERROR =>
				raise Data_Error with Message;
			when others =>
				raise Use_Error with Message;
		end case;
	end Raise_Error;
	
end YAML;
