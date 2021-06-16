with Ada.IO_Exceptions;
private with C.yaml;
private with Ada.Finalization;
package YAML is
	pragma Preelaborate;
	pragma Linker_Options ("-lyaml");
	
	function Version return String;
	
	type Version_Directive is record
		Major : Natural;
		Minor : Natural;
	end record;
	
	type Tag_Directive is record
		Handle : not null access constant String;
		Prefix : not null access constant String;
	end record;
	type Tag_Directive_Array is array (Positive range <>) of Tag_Directive;
	
	type Encoding is (Any, UTF_8, UTF_16LE, UTF_16BE);
	type Line_Break is (Any, CR, LN, CRLN);
	
	subtype Indent_Width is Integer range 1 .. 10;
	subtype Line_Width is Integer range -1 .. Integer'Last;
	
	type Mark is record
		Index : Natural;
		Line : Natural;
		Column : Natural;
	end record;
	
	type Scalar_Style is
		(Any, Plain, Single_Quoted, Double_Quoted, Literal, Folded);
	type Sequence_Style is (Any, Block, Flow);
	type Mapping_Style is (Any, Block, Flow);
	
	package Event_Types is
		type Event_Type is (
			No_Event, -- An empty event.
			Stream_Start, -- A STREAM-START event.
			Stream_End, -- A STREAM-END event.
			Document_Start, -- A DOCUMENT-START event.
			Document_End, -- A DOCUMENT-END event.
			Alias, -- An ALIAS event.
			Scalar, -- A SCALAR event.
			Sequence_Start, -- A SEQUENCE-START event.
			Sequence_End, -- A SEQUENCE-END event.
			Mapping_Start, -- A MAPPING-START event.
			Mapping_End); -- A MAPPING-END event.
	private
		for Event_Type use (
			No_Event =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_NO_EVENT),
			Stream_Start =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_STREAM_START_EVENT),
			Stream_End =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_STREAM_END_EVENT),
			Document_Start =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_DOCUMENT_START_EVENT),
			Document_End =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_DOCUMENT_END_EVENT),
			Alias =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_ALIAS_EVENT),
			Scalar =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_SCALAR_EVENT),
			Sequence_Start =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_SEQUENCE_START_EVENT),
			Sequence_End =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_SEQUENCE_END_EVENT),
			Mapping_Start =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_MAPPING_START_EVENT),
			Mapping_End =>
				C.yaml.yaml_event_type_t'Enum_Rep (C.yaml.YAML_MAPPING_END_EVENT));
	end Event_Types;
	type Event_Type is new Event_Types.Event_Type;
	
	type Event (Event_Type : YAML.Event_Type := No_Event) is record
		case Event_Type is
			when Stream_Start =>
				Encoding : YAML.Encoding;
			when Document_Start | Document_End =>
				Implicit_Indicator : Boolean;
				case Event_Type is
					when Document_Start =>
						Version_Directive : access constant YAML.Version_Directive;
						Tag_Directives : access constant YAML.Tag_Directive_Array;
					when others =>
						null;
				end case;
			when Alias | Scalar | Sequence_Start | Mapping_Start =>
				Anchor : access constant String;
				case Event_Type is
					when Scalar | Sequence_Start | Mapping_Start =>
						Tag : access constant String;
						case Event_Type is
							when Scalar =>
								Value : not null access constant String;
								Plain_Implicit_Tag : Boolean;
								Quoted_Implicit_Tag : Boolean;
								Scalar_Style : YAML.Scalar_Style;
							when Sequence_Start | Mapping_Start =>
								Implicit_Tag : Boolean;
								case Event_Type is
									when Sequence_Start =>
										Sequence_Style : YAML.Sequence_Style;
									when Mapping_Start =>
										Mapping_Style : YAML.Mapping_Style;
									when others =>
										null;
								end case;
							when others =>
								null;
						end case;
					when others =>
						null;
				end case;
			when No_Event | Stream_End | Sequence_End | Mapping_End =>
				null;
		end case;
	end record;
	
	Null_Tag : constant String := "tag:yaml.org,2002:null";
	Boolean_Tag : constant String := "tag:yaml.org,2002:bool";
	String_Tag : constant String := "tag:yaml.org,2002:str";
	Integer_Tag : constant String := "tag:yaml.org,2002:int";
	Float_Tag : constant String := "tag:yaml.org,2002:float";
	Time_Tag : constant String := "tag:yaml.org,2002:timestamp";
	Sequence_Tag : constant String := "tag:yaml.org,2002:seq";
	Mapping_Tag : constant String := "tag:yaml.org,2002:map";
	
	Default_Scalar_Tag : String
		renames String_Tag;
	Default_Sequence_tag : String
		renames Sequence_Tag;
	Default_Mapping_Tag : String
		renames Mapping_Tag;
	
	-- parser
	
	type Parser (<>) is limited private;
	
	function Create (
		Input : not null access procedure (Item : out String; Last : out Natural))
		return Parser;
	
	procedure Set_Encoding (
		Object : in out Parser;
		Encoding : in YAML.Encoding);
	
	procedure Parse (
		Object : in out Parser;
		Process : not null access procedure (
			Event : in YAML.Event;
			Start_Mark, End_Mark : in Mark));
	
	type Parsing_Entry_Type is limited private;
	pragma Preelaborable_Initialization (Parsing_Entry_Type);
	
	type Event_Reference_Type (
		Element : not null access constant Event) is null record
		with Implicit_Dereference => Element;
	type Mark_Reference_Type (
		Element : not null access constant Mark) is null record
		with Implicit_Dereference => Element;
	
	function Value (Parsing_Entry : aliased Parsing_Entry_Type)
		return Event_Reference_Type;
	function Start_Mark (Parsing_Entry : aliased Parsing_Entry_Type)
		return Mark_Reference_Type;
	function End_Mark (Parsing_Entry : aliased Parsing_Entry_Type)
		return Mark_Reference_Type;
	
	pragma Inline (Value);
	pragma Inline (Start_Mark);
	pragma Inline (End_Mark);
	
	procedure Parse (
		Object : in out Parser;
		Parsing_Entry : out Parsing_Entry_Type);
	
	procedure Parse_Document_Start (Object : in out Parser);
	procedure Parse_Document_End (Object : in out Parser);
	procedure Parse_Stream_Start (Object : in out Parser);
	procedure Parse_Stream_End (Object : in out Parser);
	
	-- emitter
	
	type Emitter (<>) is limited private;
	
	function Create (Output : not null access procedure (Item : in String))
		return Emitter;
	
	procedure Flush (Object : in out Emitter);
	
	procedure Set_Encoding (
		Object : in out Emitter;
		Encoding : in YAML.Encoding);
	procedure Set_Canonical (Object : in out Emitter; Canonical : in Boolean);
	procedure Set_Indent (Object : in out Emitter; Indent : in Indent_Width);
	procedure Set_Width (Object : in out Emitter; Width : in Line_Width);
	procedure Set_Unicode (Object : in out Emitter; Unicode : in Boolean);
	procedure Set_Break (Object : in out Emitter; Break : in Line_Break);
	
	procedure Emit (Object : in out Emitter; Event : in YAML.Event);
	
	-- exceptions
	
	Status_Error : exception
		renames Ada.IO_Exceptions.Status_Error;
	Use_Error : exception
		renames Ada.IO_Exceptions.Use_Error;
	Data_Error : exception
		renames Ada.IO_Exceptions.Data_Error;
	
private
	
	for Encoding use (
		Any => C.yaml.yaml_encoding_t'Enum_Rep (C.yaml.YAML_ANY_ENCODING),
		UTF_8 => C.yaml.yaml_encoding_t'Enum_Rep (C.yaml.YAML_UTF8_ENCODING),
		UTF_16LE => C.yaml.yaml_encoding_t'Enum_Rep (C.yaml.YAML_UTF16LE_ENCODING),
		UTF_16BE => C.yaml.yaml_encoding_t'Enum_Rep (C.yaml.YAML_UTF16BE_ENCODING));
	
	for Line_Break use (
		Any => C.yaml.yaml_break_t'Enum_Rep (C.yaml.YAML_ANY_BREAK),
		CR => C.yaml.yaml_break_t'Enum_Rep (C.yaml.YAML_CR_BREAK),
		LN => C.yaml.yaml_break_t'Enum_Rep (C.yaml.YAML_LN_BREAK),
		CRLN => C.yaml.yaml_break_t'Enum_Rep (C.yaml.YAML_CRLN_BREAK));
	
	for Scalar_Style use (
		Any =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_ANY_SCALAR_STYLE),
		Plain =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_PLAIN_SCALAR_STYLE),
		Single_Quoted =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_SINGLE_QUOTED_SCALAR_STYLE),
		Double_Quoted =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_DOUBLE_QUOTED_SCALAR_STYLE),
		Literal =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_LITERAL_SCALAR_STYLE),
		Folded =>
			C.yaml.yaml_scalar_style_t'Enum_Rep (C.yaml.YAML_FOLDED_SCALAR_STYLE));
	
	for Sequence_Style use (
		Any =>
			C.yaml.yaml_sequence_style_t'Enum_Rep (C.yaml.YAML_ANY_SEQUENCE_STYLE),
		Block =>
			C.yaml.yaml_sequence_style_t'Enum_Rep (C.yaml.YAML_BLOCK_SEQUENCE_STYLE),
		Flow =>
			C.yaml.yaml_sequence_style_t'Enum_Rep (C.yaml.YAML_FLOW_SEQUENCE_STYLE));
	
	for Mapping_Style use (
		Any =>
			C.yaml.yaml_mapping_style_t'Enum_Rep (C.yaml.YAML_ANY_MAPPING_STYLE),
		Block =>
			C.yaml.yaml_mapping_style_t'Enum_Rep (C.yaml.YAML_BLOCK_MAPPING_STYLE),
		Flow =>
			C.yaml.yaml_mapping_style_t'Enum_Rep (C.yaml.YAML_FLOW_MAPPING_STYLE));
	
	-- parser
	
	type String_Constraint is record
		First : Positive;
		Last : Natural;
	end record;
	pragma Suppress_Initialization (String_Constraint);
	
	type Parsed_Data_Type is limited record
		Event : aliased YAML.Event;
		Start_Mark, End_Mark : aliased Mark;
		Version_Directive : aliased YAML.Version_Directive;
		Anchor_Constraint : aliased String_Constraint;
		Tag_Constraint : aliased String_Constraint;
		Value_Constraint : aliased String_Constraint;
		yaml_event : aliased C.yaml.yaml_event_t;
		Delete : access procedure (Parsed_Data : in out Parsed_Data_Type);
	end record;
	pragma Suppress_Initialization (Parsed_Data_Type);
	
	type Parsing_Entry_Type is new Ada.Finalization.Limited_Controlled
		with record
			Data : aliased Parsed_Data_Type;
		end record;
	
	overriding procedure Finalize (Object : in out Parsing_Entry_Type);
	
	package Controlled_Parsers is
		
		type Parser is limited private;
		
		function Reference (Object : in out YAML.Parser)
			return not null access C.yaml.yaml_parser_t;
		
		pragma Inline (Reference);
		
	private
		
		type Uninitialized_yaml_parser_t is record
			X : aliased C.yaml.yaml_parser_t;
		end record;
		pragma Suppress_Initialization (Uninitialized_yaml_parser_t);
		
		type Parser is limited new Ada.Finalization.Limited_Controlled
			with record
				Raw : aliased Uninitialized_yaml_parser_t;
			end record;
		
		overriding procedure Finalize (Object : in out Parser);
		
	end Controlled_Parsers;
	
	type Parser is new Controlled_Parsers.Parser;
	
	-- emitter
	
	package Controlled_Emitters is
		
		type Emitter is limited private;
		
		function Reference (Object : in out YAML.Emitter)
			return not null access C.yaml.yaml_emitter_t;
		
		pragma Inline (Reference);
		
	private
		
		type Uninitialized_yaml_emitter_t is record
			X : aliased C.yaml.yaml_emitter_t;
		end record;
		pragma Suppress_Initialization (Uninitialized_yaml_emitter_t);
		
		type Emitter is limited new Ada.Finalization.Limited_Controlled
			with record
				Raw : aliased Uninitialized_yaml_emitter_t;
			end record;
		
		overriding procedure Finalize (Object : in out Emitter);
		
	end Controlled_Emitters;
	
	type Emitter is new Controlled_Emitters.Emitter;
	
	-- exceptions
	
	procedure Raise_Error (
		Error : in C.yaml.yaml_error_type_t;
		Problem : access constant C.char;
		Mark : access constant C.yaml.yaml_mark_t);
	pragma No_Return (Raise_Error);
	
end YAML;
