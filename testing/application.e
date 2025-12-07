note
	description: "Test application for simple_mmap"
	date: "$Date$"
	revision: "$Revision$"

class
	APPLICATION

create
	make

feature -- Initialization

	make
			-- Run tests.
		local
			l_tests: TEST_SIMPLE_MMAP
			l_passed, l_failed: INTEGER
		do
			print ("Testing SIMPLE_MMAP...%N%N")

			create l_tests

			-- Test: Anonymous mapping creation
			print ("  test_anonymous_mapping_creation: ")
			l_tests.on_prepare
			l_tests.test_anonymous_mapping_creation
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Anonymous read/write byte
			print ("  test_anonymous_read_write_byte: ")
			l_tests.on_prepare
			l_tests.test_anonymous_read_write_byte
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Anonymous read/write integer
			print ("  test_anonymous_read_write_integer: ")
			l_tests.on_prepare
			l_tests.test_anonymous_read_write_integer
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Anonymous read/write string
			print ("  test_anonymous_read_write_string: ")
			l_tests.on_prepare
			l_tests.test_anonymous_read_write_string
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Anonymous read/write bytes
			print ("  test_anonymous_read_write_bytes: ")
			l_tests.on_prepare
			l_tests.test_anonymous_read_write_bytes
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: File mapping read
			print ("  test_file_mapping_read: ")
			l_tests.on_prepare
			l_tests.test_file_mapping_read
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: File mapping write
			print ("  test_file_mapping_write: ")
			l_tests.on_prepare
			l_tests.test_file_mapping_write
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Shared mapping
			print ("  test_shared_mapping: ")
			l_tests.on_prepare
			l_tests.test_shared_mapping
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Invalid file
			print ("  test_invalid_file: ")
			l_tests.on_prepare
			l_tests.test_invalid_file
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			-- Test: Close invalidates
			print ("  test_close_invalidates: ")
			l_tests.on_prepare
			l_tests.test_close_invalidates
			l_tests.on_clean
			print ("PASSED%N")
			l_passed := l_passed + 1

			print ("%N======================================%N")
			print ("Results: " + l_passed.out + " passed, " + l_failed.out + " failed%N")
		rescue
			print ("FAILED%N")
			l_failed := l_failed + 1
			retry
		end

end
