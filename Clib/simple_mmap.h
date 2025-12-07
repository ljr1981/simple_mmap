/*
 * simple_mmap.h - Memory-mapped file access for Eiffel
 * Copyright (c) 2025 Larry Rix - MIT License
 */

#ifndef SIMPLE_MMAP_H
#define SIMPLE_MMAP_H

#include <windows.h>

/* Memory mapping handle structure */
typedef struct {
    HANDLE file_handle;
    HANDLE mapping_handle;
    void* view;
    size_t size;
    int is_writable;
    char* error_message;
} smm_mapping;

/* Create a new memory mapping from a file.
 * Returns NULL on failure.
 * If writable is non-zero, the mapping is read-write, otherwise read-only.
 */
smm_mapping* smm_create_file_mapping(const char* filepath, int writable);

/* Create an anonymous (pagefile-backed) memory mapping.
 * Returns NULL on failure.
 * size is the size in bytes.
 */
smm_mapping* smm_create_anonymous_mapping(size_t size);

/* Create a named shared memory mapping.
 * Returns NULL on failure.
 * If existing mapping with same name exists, opens it.
 */
smm_mapping* smm_create_shared_mapping(const char* name, size_t size);

/* Open an existing named shared memory mapping.
 * Returns NULL if not found.
 */
smm_mapping* smm_open_shared_mapping(const char* name);

/* Get the data pointer for direct access. */
void* smm_get_data(smm_mapping* mapping);

/* Get the size of the mapping in bytes. */
size_t smm_get_size(smm_mapping* mapping);

/* Read bytes from the mapping at offset. Returns bytes read. */
size_t smm_read(smm_mapping* mapping, size_t offset, void* buffer, size_t count);

/* Write bytes to the mapping at offset. Returns bytes written. */
size_t smm_write(smm_mapping* mapping, size_t offset, const void* buffer, size_t count);

/* Read a single byte at offset. */
unsigned char smm_read_byte(smm_mapping* mapping, size_t offset);

/* Write a single byte at offset. */
void smm_write_byte(smm_mapping* mapping, size_t offset, unsigned char value);

/* Read a 32-bit integer at offset. */
int smm_read_int32(smm_mapping* mapping, size_t offset);

/* Write a 32-bit integer at offset. */
void smm_write_int32(smm_mapping* mapping, size_t offset, int value);

/* Flush changes to disk (for file mappings). */
int smm_flush(smm_mapping* mapping);

/* Check if the mapping is valid. */
int smm_is_valid(smm_mapping* mapping);

/* Check if the mapping is writable. */
int smm_is_writable(smm_mapping* mapping);

/* Get the last error message. */
const char* smm_get_error(smm_mapping* mapping);

/* Close and free a memory mapping. */
void smm_close(smm_mapping* mapping);

#endif /* SIMPLE_MMAP_H */
