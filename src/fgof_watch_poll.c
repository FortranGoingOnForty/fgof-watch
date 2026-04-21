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

static const char *fgof_watch_basename(const char *path) {
    const char *slash;

    slash = strrchr(path, '/');
    if (slash == NULL) {
        return path;
    }
    return slash + 1;
}

static const char *fgof_watch_relative_path(const char *root, const char *path) {
    size_t root_len;

    if (strcmp(path, root) == 0) {
        return fgof_watch_basename(root);
    }

    root_len = strlen(root);
    if (strncmp(path, root, root_len) == 0 && path[root_len] == '/') {
        return path + root_len + 1;
    }

    return path;
}

static int fgof_watch_contains_hidden_segment(const char *path) {
    const char *segment_start;
    const char *cursor;

    if (path == NULL || path[0] == '\0') {
        return 0;
    }

    segment_start = path;
    for (cursor = path; ; ++cursor) {
        if (*cursor != '/' && *cursor != '\0') {
            continue;
        }
        if (cursor > segment_start && segment_start[0] == '.') {
            return 1;
        }
        if (*cursor == '\0') {
            break;
        }
        segment_start = cursor + 1;
    }

    return 0;
}

static int fgof_watch_matches_prefix(const char *path, int prefix_count, int prefix_stride, const char *prefixes) {
    int i;
    const char *prefix;
    size_t prefix_len;

    if (prefix_count <= 0 || prefix_stride <= 0 || prefixes == NULL) {
        return 0;
    }

    for (i = 0; i < prefix_count; ++i) {
        prefix = prefixes + (i * prefix_stride);
        prefix_len = strlen(prefix);
        if (prefix_len == 0) {
            continue;
        }
        if (strcmp(path, prefix) == 0) {
            return 1;
        }
        if (strncmp(path, prefix, prefix_len) == 0 && path[prefix_len] == '/') {
            return 1;
        }
    }

    return 0;
}

static int fgof_watch_should_ignore(
    const char *root,
    const char *path,
    int ignore_hidden,
    int prefix_count,
    int prefix_stride,
    const char *prefixes
) {
    if (ignore_hidden && fgof_watch_contains_hidden_segment(fgof_watch_relative_path(root, path))) {
        return 1;
    }

    if (fgof_watch_matches_prefix(path, prefix_count, prefix_stride, prefixes)) {
        return 1;
    }

    return 0;
}

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

static int fgof_watch_visit(
    const char *root,
    const char *path,
    int recursive,
    int depth,
    int ignore_hidden,
    int prefix_count,
    int prefix_stride,
    const char *prefixes,
    fgof_watch_buffer *buffer
) {
    DIR *dirp;
    struct dirent *entry;
    struct stat st;

    if (fgof_watch_should_ignore(root, path, ignore_hidden, prefix_count, prefix_stride, prefixes)) {
        return 0;
    }

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
        status = fgof_watch_visit(
            root,
            child,
            recursive,
            depth + 1,
            ignore_hidden,
            prefix_count,
            prefix_stride,
            prefixes,
            buffer
        );
        free(child);

        if (status != 0) {
            closedir(dirp);
            return status;
        }
    }

    closedir(dirp);
    return 0;
}

int fgof_watch_collect_snapshot(
    const char *root,
    int recursive,
    int ignore_hidden,
    int prefix_count,
    int prefix_stride,
    const char *prefixes,
    char **buffer_out,
    size_t *buffer_len_out
) {
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

    status = fgof_watch_visit(
        root,
        root,
        recursive != 0,
        0,
        ignore_hidden != 0,
        prefix_count,
        prefix_stride,
        prefixes,
        &buffer
    );
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
