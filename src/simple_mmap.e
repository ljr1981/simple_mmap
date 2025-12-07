note
	description: "[
		SCOOP-compatible memory-mapped file access.
		Uses direct Win32 API calls via C wrapper.
	]"
	author: "Larry Rix"
	date: "$Date$"
	revision: "$Revision$"

class
	SIMPLE_MMAP

create
	make_from_file,
	make_anonymous,
	make_shared,
	open_shared

feature {NONE} -- Initialization

	make_from_file (a_path: READABLE_STRING_GENERAL; a_writable: BOOLEAN)
			-- Create mapping from file at `a_path'.
			-- If `a_writable', mapping is read-write.
		require
			path_not_empty: not a_path.is_empty
		local
			l_path: C_STRING
		do
			create l_path.make (a_path.to_string_8)
			if a_writable then
				handle := c_smm_create_file_mapping (l_path.item, 1)
			else
				handle := c_smm_create_file_mapping (l_path.item, 0)
			end
		end

	make_anonymous (a_size: INTEGER)
			-- Create anonymous (pagefile-backed) mapping of `a_size' bytes.
		require
			positive_size: a_size > 0
		do
			handle := c_smm_create_anonymous_mapping (a_size.to_natural_64)
		end

	make_shared (a_name: READABLE_STRING_GENERAL; a_size: INTEGER)
			-- Create or open named shared mapping with `a_name' of `a_size' bytes.
		require
			name_not_empty: not a_name.is_empty
			positive_size: a_size > 0
		local
			l_name: C_STRING
		do
			create l_name.make (a_name.to_string_8)
			handle := c_smm_create_shared_mapping (l_name.item, a_size.to_natural_64)
		end

	open_shared (a_name: READABLE_STRING_GENERAL)
			-- Open existing named shared mapping with `a_name'.
		require
			name_not_empty: not a_name.is_empty
		local
			l_name: C_STRING
		do
			create l_name.make (a_name.to_string_8)
			handle := c_smm_open_shared_mapping (l_name.item)
		end

feature -- Status

	is_valid: BOOLEAN
			-- Is the mapping valid and usable?
		do
			Result := handle /= default_pointer and then c_smm_is_valid (handle) /= 0
		end

	is_writable: BOOLEAN
			-- Is the mapping writable?
		do
			Result := handle /= default_pointer and then c_smm_is_writable (handle) /= 0
		end

	size: INTEGER
			-- Size of the mapping in bytes.
		do
			if handle /= default_pointer then
				Result := c_smm_get_size (handle).to_integer_32
			end
		end

	last_error: detachable STRING_32
			-- Error message from last failed operation.
		local
			l_ptr: POINTER
			l_c_string: C_STRING
		do
			if handle /= default_pointer then
				l_ptr := c_smm_get_error (handle)
				if l_ptr /= default_pointer then
					create l_c_string.make_by_pointer (l_ptr)
					Result := l_c_string.string.to_string_32
				end
			end
		end

feature -- Read Operations

	read_byte (a_offset: INTEGER): NATURAL_8
			-- Read byte at `a_offset'.
		require
			valid: is_valid
			valid_offset: a_offset >= 0 and a_offset < size
		do
			Result := c_smm_read_byte (handle, a_offset.to_natural_64)
		end

	read_integer (a_offset: INTEGER): INTEGER
			-- Read 32-bit integer at `a_offset'.
		require
			valid: is_valid
			valid_offset: a_offset >= 0 and a_offset + 4 <= size
		do
			Result := c_smm_read_int32 (handle, a_offset.to_natural_64)
		end

	read_bytes (a_offset, a_count: INTEGER): ARRAY [NATURAL_8]
			-- Read `a_count' bytes starting at `a_offset'.
		require
			valid: is_valid
			valid_offset: a_offset >= 0
			positive_count: a_count > 0
			in_bounds: a_offset + a_count <= size
		local
			l_managed: MANAGED_POINTER
			l_read: INTEGER
			i: INTEGER
		do
			create l_managed.make (a_count)
			l_read := c_smm_read (handle, a_offset.to_natural_64, l_managed.item, a_count.to_natural_64).to_integer_32
			create Result.make_filled (0, 1, l_read)
			from i := 0 until i >= l_read loop
				Result.put (l_managed.read_natural_8 (i), i + 1)
				i := i + 1
			end
		end

	read_string (a_offset, a_max_length: INTEGER): STRING_8
			-- Read null-terminated string starting at `a_offset', up to `a_max_length' bytes.
		require
			valid: is_valid
			valid_offset: a_offset >= 0
			positive_length: a_max_length > 0
		local
			l_bytes: ARRAY [NATURAL_8]
			i: INTEGER
			l_byte: NATURAL_8
		do
			l_bytes := read_bytes (a_offset, a_max_length.min (size - a_offset))
			create Result.make (l_bytes.count)
			from i := 1 until i > l_bytes.count loop
				l_byte := l_bytes.item (i)
				if l_byte = 0 then
					i := l_bytes.count + 1 -- exit loop
				else
					Result.append_character (l_byte.to_character_8)
					i := i + 1
				end
			end
		end

feature -- Write Operations

	write_byte (a_offset: INTEGER; a_value: NATURAL_8)
			-- Write `a_value' at `a_offset'.
		require
			valid: is_valid
			writable: is_writable
			valid_offset: a_offset >= 0 and a_offset < size
		do
			c_smm_write_byte (handle, a_offset.to_natural_64, a_value)
		end

	write_integer (a_offset: INTEGER; a_value: INTEGER)
			-- Write 32-bit `a_value' at `a_offset'.
		require
			valid: is_valid
			writable: is_writable
			valid_offset: a_offset >= 0 and a_offset + 4 <= size
		do
			c_smm_write_int32 (handle, a_offset.to_natural_64, a_value)
		end

	write_bytes (a_offset: INTEGER; a_bytes: ARRAY [NATURAL_8])
			-- Write `a_bytes' starting at `a_offset'.
		require
			valid: is_valid
			writable: is_writable
			valid_offset: a_offset >= 0
			bytes_not_empty: not a_bytes.is_empty
			in_bounds: a_offset + a_bytes.count <= size
		local
			l_managed: MANAGED_POINTER
			i: INTEGER
		do
			create l_managed.make (a_bytes.count)
			from i := a_bytes.lower until i > a_bytes.upper loop
				l_managed.put_natural_8 (a_bytes.item (i), i - a_bytes.lower)
				i := i + 1
			end
			last_write_count := c_smm_write (handle, a_offset.to_natural_64, l_managed.item, a_bytes.count.to_natural_64).to_integer_32
		end

	write_string (a_offset: INTEGER; a_string: READABLE_STRING_8)
			-- Write `a_string' (with null terminator) starting at `a_offset'.
		require
			valid: is_valid
			writable: is_writable
			valid_offset: a_offset >= 0
			string_not_empty: not a_string.is_empty
			in_bounds: a_offset + a_string.count + 1 <= size
		local
			l_bytes: ARRAY [NATURAL_8]
			i: INTEGER
		do
			create l_bytes.make_filled (0, 1, a_string.count + 1)
			from i := 1 until i > a_string.count loop
				l_bytes.put (a_string.item (i).code.to_natural_8, i)
				i := i + 1
			end
			l_bytes.put (0, a_string.count + 1) -- null terminator
			write_bytes (a_offset, l_bytes)
		end

feature -- Operations

	flush
			-- Flush changes to disk (for file mappings).
		require
			valid: is_valid
		do
			last_flush_succeeded := c_smm_flush (handle) /= 0
		end

	close
			-- Close and release the mapping.
		do
			if handle /= default_pointer then
				c_smm_close (handle)
				handle := default_pointer
			end
		ensure
			closed: handle = default_pointer
		end

feature -- Status Report

	last_write_count: INTEGER
			-- Number of bytes written in last write operation.

	last_flush_succeeded: BOOLEAN
			-- Did the last flush succeed?

feature {NONE} -- Implementation

	handle: POINTER
			-- C handle to the mapping.

feature {NONE} -- C Externals

	c_smm_create_file_mapping (a_path: POINTER; a_writable: INTEGER): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_create_file_mapping((const char*)$a_path, $a_writable);"
		end

	c_smm_create_anonymous_mapping (a_size: NATURAL_64): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_create_anonymous_mapping((size_t)$a_size);"
		end

	c_smm_create_shared_mapping (a_name: POINTER; a_size: NATURAL_64): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_create_shared_mapping((const char*)$a_name, (size_t)$a_size);"
		end

	c_smm_open_shared_mapping (a_name: POINTER): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_open_shared_mapping((const char*)$a_name);"
		end

	c_smm_get_data (a_handle: POINTER): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_get_data((smm_mapping*)$a_handle);"
		end

	c_smm_get_size (a_handle: POINTER): NATURAL_64
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return (EIF_NATURAL_64)smm_get_size((smm_mapping*)$a_handle);"
		end

	c_smm_read (a_handle: POINTER; a_offset: NATURAL_64; a_buffer: POINTER; a_count: NATURAL_64): NATURAL_64
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return (EIF_NATURAL_64)smm_read((smm_mapping*)$a_handle, (size_t)$a_offset, $a_buffer, (size_t)$a_count);"
		end

	c_smm_write (a_handle: POINTER; a_offset: NATURAL_64; a_buffer: POINTER; a_count: NATURAL_64): NATURAL_64
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return (EIF_NATURAL_64)smm_write((smm_mapping*)$a_handle, (size_t)$a_offset, $a_buffer, (size_t)$a_count);"
		end

	c_smm_read_byte (a_handle: POINTER; a_offset: NATURAL_64): NATURAL_8
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_read_byte((smm_mapping*)$a_handle, (size_t)$a_offset);"
		end

	c_smm_write_byte (a_handle: POINTER; a_offset: NATURAL_64; a_value: NATURAL_8)
		external
			"C inline use %"simple_mmap.h%""
		alias
			"smm_write_byte((smm_mapping*)$a_handle, (size_t)$a_offset, $a_value);"
		end

	c_smm_read_int32 (a_handle: POINTER; a_offset: NATURAL_64): INTEGER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_read_int32((smm_mapping*)$a_handle, (size_t)$a_offset);"
		end

	c_smm_write_int32 (a_handle: POINTER; a_offset: NATURAL_64; a_value: INTEGER)
		external
			"C inline use %"simple_mmap.h%""
		alias
			"smm_write_int32((smm_mapping*)$a_handle, (size_t)$a_offset, $a_value);"
		end

	c_smm_flush (a_handle: POINTER): INTEGER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_flush((smm_mapping*)$a_handle);"
		end

	c_smm_is_valid (a_handle: POINTER): INTEGER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_is_valid((smm_mapping*)$a_handle);"
		end

	c_smm_is_writable (a_handle: POINTER): INTEGER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return smm_is_writable((smm_mapping*)$a_handle);"
		end

	c_smm_get_error (a_handle: POINTER): POINTER
		external
			"C inline use %"simple_mmap.h%""
		alias
			"return (char*)smm_get_error((smm_mapping*)$a_handle);"
		end

	c_smm_close (a_handle: POINTER)
		external
			"C inline use %"simple_mmap.h%""
		alias
			"smm_close((smm_mapping*)$a_handle);"
		end

invariant
	handle_default_implies_invalid: handle = default_pointer implies not is_valid

end
