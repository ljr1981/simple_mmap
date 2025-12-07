note
	description: "Tests for SIMPLE_MMAP library"
	testing: "type/manual"

class
	TEST_SIMPLE_MMAP

inherit
	TEST_SET_BASE
		redefine
			on_prepare,
			on_clean
		end

feature -- Setup

	on_prepare
			-- Set up test fixtures.
		do
			-- Nothing to prepare
		end

	on_clean
			-- Clean up after tests.
		local
			l_file: RAW_FILE
		do
			-- Clean up test file
			create l_file.make_with_name (test_file_path)
			if l_file.exists then
				l_file.delete
			end
		end

feature -- Access

	test_file_path: STRING = "simple_mmap_test.dat"
			-- Path to test file.

feature -- Test: Anonymous Mapping

	test_anonymous_mapping_creation
			-- Test creating anonymous mapping.
		local
			l_mmap: SIMPLE_MMAP
		do
			create l_mmap.make_anonymous (4096)
			assert ("mapping valid", l_mmap.is_valid)
			assert ("mapping writable", l_mmap.is_writable)
			assert ("correct size", l_mmap.size = 4096)
			l_mmap.close
		end

	test_anonymous_read_write_byte
			-- Test reading and writing bytes in anonymous mapping.
		local
			l_mmap: SIMPLE_MMAP
		do
			create l_mmap.make_anonymous (1024)
			assert ("valid", l_mmap.is_valid)

			l_mmap.write_byte (0, 42)
			l_mmap.write_byte (1, 255)
			l_mmap.write_byte (100, 128)

			assert ("byte 0", l_mmap.read_byte (0) = 42)
			assert ("byte 1", l_mmap.read_byte (1) = 255)
			assert ("byte 100", l_mmap.read_byte (100) = 128)

			l_mmap.close
		end

	test_anonymous_read_write_integer
			-- Test reading and writing integers in anonymous mapping.
		local
			l_mmap: SIMPLE_MMAP
		do
			create l_mmap.make_anonymous (1024)
			assert ("valid", l_mmap.is_valid)

			l_mmap.write_integer (0, 12345)
			l_mmap.write_integer (4, -9999)
			l_mmap.write_integer (100, 2147483647)

			assert ("int at 0", l_mmap.read_integer (0) = 12345)
			assert ("int at 4", l_mmap.read_integer (4) = -9999)
			assert ("int at 100", l_mmap.read_integer (100) = 2147483647)

			l_mmap.close
		end

	test_anonymous_read_write_string
			-- Test reading and writing strings in anonymous mapping.
		local
			l_mmap: SIMPLE_MMAP
			l_str: STRING_8
		do
			create l_mmap.make_anonymous (1024)
			assert ("valid", l_mmap.is_valid)

			l_mmap.write_string (0, "Hello, World!")
			l_str := l_mmap.read_string (0, 100)
			assert ("string correct", l_str.same_string ("Hello, World!"))

			l_mmap.write_string (100, "Test String")
			l_str := l_mmap.read_string (100, 50)
			assert ("string at 100", l_str.same_string ("Test String"))

			l_mmap.close
		end

	test_anonymous_read_write_bytes
			-- Test reading and writing byte arrays in anonymous mapping.
		local
			l_mmap: SIMPLE_MMAP
			l_write_bytes, l_read_bytes: ARRAY [NATURAL_8]
		do
			create l_mmap.make_anonymous (1024)
			assert ("valid", l_mmap.is_valid)

			create l_write_bytes.make_filled (0, 1, 5)
			l_write_bytes.put (1, 1)
			l_write_bytes.put (2, 2)
			l_write_bytes.put (3, 3)
			l_write_bytes.put (4, 4)
			l_write_bytes.put (5, 5)

			l_mmap.write_bytes (50, l_write_bytes)
			l_read_bytes := l_mmap.read_bytes (50, 5)

			assert ("count correct", l_read_bytes.count = 5)
			assert ("byte 1", l_read_bytes.item (1) = 1)
			assert ("byte 2", l_read_bytes.item (2) = 2)
			assert ("byte 3", l_read_bytes.item (3) = 3)
			assert ("byte 4", l_read_bytes.item (4) = 4)
			assert ("byte 5", l_read_bytes.item (5) = 5)

			l_mmap.close
		end

feature -- Test: File Mapping

	test_file_mapping_read
			-- Test creating file mapping for reading.
		local
			l_file: RAW_FILE
			l_mmap: SIMPLE_MMAP
			l_byte: NATURAL_8
		do
			-- Create test file
			create l_file.make_create_read_write (test_file_path)
			l_file.put_character ('A')
			l_file.put_character ('B')
			l_file.put_character ('C')
			l_file.close

			-- Map the file
			create l_mmap.make_from_file (test_file_path, False)
			assert ("mapping valid", l_mmap.is_valid)
			assert ("not writable", not l_mmap.is_writable)
			assert ("size is 3", l_mmap.size = 3)

			l_byte := l_mmap.read_byte (0)
			assert ("first byte is A", l_byte = 65) -- 'A' = 65

			l_byte := l_mmap.read_byte (1)
			assert ("second byte is B", l_byte = 66)

			l_byte := l_mmap.read_byte (2)
			assert ("third byte is C", l_byte = 67)

			l_mmap.close

			-- Clean up
			create l_file.make_with_name (test_file_path)
			l_file.delete
		end

	test_file_mapping_write
			-- Test creating file mapping for writing.
		local
			l_file: RAW_FILE
			l_mmap: SIMPLE_MMAP
		do
			-- Create test file with some content
			create l_file.make_create_read_write (test_file_path)
			l_file.put_string ("XXXX")
			l_file.close

			-- Map the file for writing
			create l_mmap.make_from_file (test_file_path, True)
			assert ("mapping valid", l_mmap.is_valid)
			assert ("is writable", l_mmap.is_writable)

			-- Modify content
			l_mmap.write_byte (0, 65) -- 'A'
			l_mmap.write_byte (1, 66) -- 'B'
			l_mmap.write_byte (2, 67) -- 'C'
			l_mmap.write_byte (3, 68) -- 'D'
			l_mmap.flush
			l_mmap.close

			-- Verify changes persisted
			create l_file.make_open_read (test_file_path)
			assert ("file has content", l_file.count = 4)
			l_file.read_character
			assert ("first char A", l_file.last_character = 'A')
			l_file.read_character
			assert ("second char B", l_file.last_character = 'B')
			l_file.read_character
			assert ("third char C", l_file.last_character = 'C')
			l_file.read_character
			assert ("fourth char D", l_file.last_character = 'D')
			l_file.close

			-- Clean up
			create l_file.make_with_name (test_file_path)
			l_file.delete
		end

feature -- Test: Shared Mapping

	test_shared_mapping
			-- Test creating and opening shared memory.
		local
			l_mmap1, l_mmap2: SIMPLE_MMAP
		do
			-- Create shared mapping
			create l_mmap1.make_shared ("SimpleMMAPTestShared", 1024)
			assert ("mmap1 valid", l_mmap1.is_valid)

			-- Write some data
			l_mmap1.write_integer (0, 42)
			l_mmap1.write_string (4, "Shared!")

			-- Open from another handle
			create l_mmap2.open_shared ("SimpleMMAPTestShared")
			assert ("mmap2 valid", l_mmap2.is_valid)

			-- Verify data is visible
			assert ("shared int", l_mmap2.read_integer (0) = 42)
			assert ("shared string", l_mmap2.read_string (4, 20).same_string ("Shared!"))

			l_mmap2.close
			l_mmap1.close
		end

feature -- Test: Error Handling

	test_invalid_file
			-- Test mapping nonexistent file.
		local
			l_mmap: SIMPLE_MMAP
		do
			create l_mmap.make_from_file ("nonexistent_file_12345.xyz", False)
			assert ("not valid", not l_mmap.is_valid)
			assert ("has error", l_mmap.last_error /= Void)
			l_mmap.close
		end

	test_close_invalidates
			-- Test that close invalidates the mapping.
		local
			l_mmap: SIMPLE_MMAP
		do
			create l_mmap.make_anonymous (1024)
			assert ("initially valid", l_mmap.is_valid)
			l_mmap.close
			assert ("invalid after close", not l_mmap.is_valid)
		end

end
