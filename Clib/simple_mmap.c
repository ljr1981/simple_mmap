/*
 * simple_mmap.c - Memory-mapped file access for Eiffel
 * Copyright (c) 2025 Larry Rix - MIT License
 */

#include "simple_mmap.h"
#include <stdlib.h>
#include <string.h>

static char last_error_msg[512] = {0};

static void store_last_error(void) {
    DWORD err = GetLastError();
    FormatMessageA(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, err,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        last_error_msg, sizeof(last_error_msg) - 1, NULL
    );
}

static smm_mapping* create_mapping_struct(void) {
    smm_mapping* m = (smm_mapping*)malloc(sizeof(smm_mapping));
    if (m) {
        memset(m, 0, sizeof(smm_mapping));
        m->file_handle = INVALID_HANDLE_VALUE;
        m->mapping_handle = NULL;
        m->view = NULL;
    }
    return m;
}

smm_mapping* smm_create_file_mapping(const char* filepath, int writable) {
    smm_mapping* m;
    DWORD access, share, protect, map_access;
    LARGE_INTEGER file_size;

    m = create_mapping_struct();
    if (!m) return NULL;

    if (writable) {
        access = GENERIC_READ | GENERIC_WRITE;
        share = 0;
        protect = PAGE_READWRITE;
        map_access = FILE_MAP_WRITE;
        m->is_writable = 1;
    } else {
        access = GENERIC_READ;
        share = FILE_SHARE_READ;
        protect = PAGE_READONLY;
        map_access = FILE_MAP_READ;
        m->is_writable = 0;
    }

    /* Open file */
    m->file_handle = CreateFileA(filepath, access, share, NULL, OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL, NULL);
    if (m->file_handle == INVALID_HANDLE_VALUE) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        return m;
    }

    /* Get file size */
    if (!GetFileSizeEx(m->file_handle, &file_size)) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->file_handle);
        m->file_handle = INVALID_HANDLE_VALUE;
        return m;
    }
    m->size = (size_t)file_size.QuadPart;

    /* Create mapping */
    m->mapping_handle = CreateFileMappingA(m->file_handle, NULL, protect, 0, 0, NULL);
    if (!m->mapping_handle) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->file_handle);
        m->file_handle = INVALID_HANDLE_VALUE;
        return m;
    }

    /* Map view */
    m->view = MapViewOfFile(m->mapping_handle, map_access, 0, 0, 0);
    if (!m->view) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->mapping_handle);
        m->mapping_handle = NULL;
        CloseHandle(m->file_handle);
        m->file_handle = INVALID_HANDLE_VALUE;
        return m;
    }

    return m;
}

smm_mapping* smm_create_anonymous_mapping(size_t size) {
    smm_mapping* m;
    DWORD size_high, size_low;

    m = create_mapping_struct();
    if (!m) return NULL;

    m->is_writable = 1;
    m->size = size;

    size_high = (DWORD)(size >> 32);
    size_low = (DWORD)(size & 0xFFFFFFFF);

    /* Create pagefile-backed mapping */
    m->mapping_handle = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL,
                                           PAGE_READWRITE, size_high, size_low, NULL);
    if (!m->mapping_handle) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        return m;
    }

    /* Map view */
    m->view = MapViewOfFile(m->mapping_handle, FILE_MAP_WRITE, 0, 0, 0);
    if (!m->view) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->mapping_handle);
        m->mapping_handle = NULL;
        return m;
    }

    return m;
}

smm_mapping* smm_create_shared_mapping(const char* name, size_t size) {
    smm_mapping* m;
    DWORD size_high, size_low;

    m = create_mapping_struct();
    if (!m) return NULL;

    m->is_writable = 1;
    m->size = size;

    size_high = (DWORD)(size >> 32);
    size_low = (DWORD)(size & 0xFFFFFFFF);

    /* Create or open named mapping */
    m->mapping_handle = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL,
                                           PAGE_READWRITE, size_high, size_low, name);
    if (!m->mapping_handle) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        return m;
    }

    /* Map view */
    m->view = MapViewOfFile(m->mapping_handle, FILE_MAP_WRITE, 0, 0, size);
    if (!m->view) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->mapping_handle);
        m->mapping_handle = NULL;
        return m;
    }

    return m;
}

smm_mapping* smm_open_shared_mapping(const char* name) {
    smm_mapping* m;
    MEMORY_BASIC_INFORMATION mem_info;

    m = create_mapping_struct();
    if (!m) return NULL;

    m->is_writable = 1;

    /* Open existing named mapping */
    m->mapping_handle = OpenFileMappingA(FILE_MAP_WRITE, FALSE, name);
    if (!m->mapping_handle) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        return m;
    }

    /* Map view */
    m->view = MapViewOfFile(m->mapping_handle, FILE_MAP_WRITE, 0, 0, 0);
    if (!m->view) {
        store_last_error();
        m->error_message = _strdup(last_error_msg);
        CloseHandle(m->mapping_handle);
        m->mapping_handle = NULL;
        return m;
    }

    /* Get size from memory info */
    if (VirtualQuery(m->view, &mem_info, sizeof(mem_info))) {
        m->size = mem_info.RegionSize;
    }

    return m;
}

void* smm_get_data(smm_mapping* mapping) {
    return mapping ? mapping->view : NULL;
}

size_t smm_get_size(smm_mapping* mapping) {
    return mapping ? mapping->size : 0;
}

size_t smm_read(smm_mapping* mapping, size_t offset, void* buffer, size_t count) {
    size_t available;

    if (!mapping || !mapping->view || !buffer) return 0;
    if (offset >= mapping->size) return 0;

    available = mapping->size - offset;
    if (count > available) count = available;

    memcpy(buffer, (char*)mapping->view + offset, count);
    return count;
}

size_t smm_write(smm_mapping* mapping, size_t offset, const void* buffer, size_t count) {
    size_t available;

    if (!mapping || !mapping->view || !buffer) return 0;
    if (!mapping->is_writable) return 0;
    if (offset >= mapping->size) return 0;

    available = mapping->size - offset;
    if (count > available) count = available;

    memcpy((char*)mapping->view + offset, buffer, count);
    return count;
}

unsigned char smm_read_byte(smm_mapping* mapping, size_t offset) {
    if (!mapping || !mapping->view) return 0;
    if (offset >= mapping->size) return 0;
    return *((unsigned char*)mapping->view + offset);
}

void smm_write_byte(smm_mapping* mapping, size_t offset, unsigned char value) {
    if (!mapping || !mapping->view || !mapping->is_writable) return;
    if (offset >= mapping->size) return;
    *((unsigned char*)mapping->view + offset) = value;
}

int smm_read_int32(smm_mapping* mapping, size_t offset) {
    if (!mapping || !mapping->view) return 0;
    if (offset + sizeof(int) > mapping->size) return 0;
    return *(int*)((char*)mapping->view + offset);
}

void smm_write_int32(smm_mapping* mapping, size_t offset, int value) {
    if (!mapping || !mapping->view || !mapping->is_writable) return;
    if (offset + sizeof(int) > mapping->size) return;
    *(int*)((char*)mapping->view + offset) = value;
}

int smm_flush(smm_mapping* mapping) {
    if (!mapping || !mapping->view) return 0;
    return FlushViewOfFile(mapping->view, 0) ? 1 : 0;
}

int smm_is_valid(smm_mapping* mapping) {
    return (mapping && mapping->view) ? 1 : 0;
}

int smm_is_writable(smm_mapping* mapping) {
    return mapping ? mapping->is_writable : 0;
}

const char* smm_get_error(smm_mapping* mapping) {
    return mapping ? mapping->error_message : NULL;
}

void smm_close(smm_mapping* mapping) {
    if (mapping) {
        if (mapping->view) {
            UnmapViewOfFile(mapping->view);
        }
        if (mapping->mapping_handle) {
            CloseHandle(mapping->mapping_handle);
        }
        if (mapping->file_handle != INVALID_HANDLE_VALUE) {
            CloseHandle(mapping->file_handle);
        }
        if (mapping->error_message) {
            free(mapping->error_message);
        }
        free(mapping);
    }
}
