/* $Revision: 1.1 $
 */

void *safe_malloc(size_t);
void *safe_realloc(void *, size_t);

void set_alloc_failure(void (*)(char *, size_t));
