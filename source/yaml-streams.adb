with Ada.Unchecked_Conversion;
package body YAML.Streams is
	use type C.signed_int;
	
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
		type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
		function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Stream_Access);
		Stream : constant Stream_Access := Cast (data);
		Ada_Data : Ada.Streams.Stream_Element_Array (
			1 ..
			Ada.Streams.Stream_Element_Offset (size));
		for Ada_Data'Address use buffer.all'Address;
		Last : Ada.Streams.Stream_Element_Offset;
	begin
		Ada.Streams.Read (Stream.all, Ada_Data, Last);
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
		type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
		function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Stream_Access);
		Stream : constant Stream_Access := Cast (data);
		Ada_Data : Ada.Streams.Stream_Element_Array (
			1 ..
			Ada.Streams.Stream_Element_Offset (size));
		for Ada_Data'Address use buffer.all'Address;
	begin
		Ada.Streams.Write (Stream.all, Ada_Data);
		return 1;
	end Write_Handler;
	
	-- implementation
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Parser is
	begin
		return Result : Parser do
			if C.yaml.yaml_parser_initialize (Result.Raw'Access) = 0 then
				Raise_Error (Result.Raw.error, Result.Raw.problem, null);
			end if;
			C.yaml.yaml_parser_set_input (
				Result.Raw'Access,
				Read_Handler'Access,
				C.void_ptr (Stream.all'Address));
		end return;
	end Create;
	
	function Create (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Emitter is
	begin
		return Result : Emitter do
			if C.yaml.yaml_emitter_initialize (Result.Raw'Access) = 0 then
				Raise_Error (Result.Raw.error, Result.Raw.problem, null);
			end if;
			C.yaml.yaml_emitter_set_output (
				Result.Raw'Access,
				Write_Handler'Access,
				C.void_ptr (Stream.all'Address));
		end return;
	end Create;
	
end YAML.Streams;
