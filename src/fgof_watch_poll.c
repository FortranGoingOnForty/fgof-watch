#include <dirent.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} fgof_watch_buffer;

static int fgof_watch_append_text(fgof_watch_buffer *buffer, const char *text, size_t text_len) {
    char *grown;
    size_t needed;
    size_t cap;

    needed = buffer->len + text_len;
    if (needed <= buffer->cap) {
        memcpy(buffer->data + buffer->len, text, text_len);
        buffer->len += text_len;
        return 0;
    }

    cap = (buffer->cap == 0) ? 1024 : buffer->cap;
    while (cap < needed) {
        cap *= 2;
    }

    grown = (char *)realloc(buffer->data, cap);
    if (grown == NULL) {
        return ENOMEM;
    }

    buffer->data = grown;
    buffer->cap = cap;
    memcpy(buffer->data + buffer->len, text, text_len);
    buffer->len += text_len;
    return 0;
}

static int fgof_watch_append_entry(fgof_watch_buffer *buffer, const char *path, const struct stat *st) {
    char line[4096];
    char kind;
    int line_len;
    long long mtime_sec;
    long long mtime_nsec;

    if (S_ISDIR(st->st_mode)) {
        kind = 'D';
    } else {
        kind = 'F';
    }

#if defined(__APPLE__)
    mtime_sec = (long long)st->st_mtimespec.tv_sec;
    mtime_nsec = (long long)st->st_mtimespec.tv_nsec;
#else
    mtime_sec = (long long)st->st_mtim.tv_sec;
    mtime_nsec = (long long)st->st_mtim.tv_nsec;
#endif

    line_len = snprintf(
        line,
        sizeof(line),
        "%c\t%lld\t%lld\t%lld\t%lld\t%s\n",
        kind,
        (long long)st->st_ino,
        (long long)st->st_size,
        mtime_sec,
        mtime_nsec,
        path
    );

    if (line_len < 0) {
        return EINVAL;
    }
    if ((size_t)line_len >= sizeof(line)) {
        return EOVERFLOW;
    }

    return fgof_watch_append_text(buffer, line, (size_t)line_len);
}

static int fgof_watch_visit(const char *path, int recursive, int depth, fgof_watch_buffer *buffer) {
    DIR *dirp;
    struct dirent *entry;
    struct stat st;

    if (lstat(path, &st) != 0) {
        if (errno == ENOENT || errno == ENOTDIR) {
            return 0;
        }
        return errno;
    }

    if (fgof_watch_append_entry(buffer, path, &st) != 0) {
        return -1;
    }

    if (!S_ISDIR(st.st_mode)) {
        return 0;
    }

    if (!recursive && depth > 0) {
        return 0;
    }

    dirp = opendir(path);
    if (dirp == NULL) {
        if (errno == ENOENT || errno == ENOTDIR) {
            return 0;
        }
        return errno;
    }

    while ((entry = readdir(dirp)) != NULL) {
        char *child;
        size_t child_len;
        int status;

        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        child_len = strlen(path) + 1 + strlen(entry->d_name) + 1;
        child = (char *)malloc(child_len);
        if (child == NULL) {
            closedir(dirp);
            return ENOMEM;
        }

        snprintf(child, child_len, "%s/%s", path, entry->d_name);
        status = fgof_watch_visit(child, recursive, depth + 1, buffer);
        free(child);

        if (status != 0) {
            closedir(dirp);
            return status;
        }
    }

    closedir(dirp);
    return 0;
}

int fgof_watch_collect_snapshot(const char *root, int recursive, char **buffer_out, size_t *buffer_len_out) {
    fgof_watch_buffer buffer;
    int status;

    buffer.data = NULL;
    buffer.len = 0;
    buffer.cap = 0;

    *buffer_out = NULL;
    *buffer_len_out = 0;

    if (root == NULL || root[0] == '\0') {
        return 0;
    }

    status = fgof_watch_visit(root, recursive != 0, 0, &buffer);
    if (status != 0) {
        free(buffer.data);
        return status;
    }

    *buffer_out = buffer.data;
    *buffer_len_out = buffer.len;
    return 0;
}

void fgof_watch_free_buffer(char *buffer) {
    free(buffer);
}
