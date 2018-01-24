#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  TSNode node;
  const char *type;
  TSSymbol symbol;
  TSPoint startPoint;
  TSPoint endPoint;
  uint32_t startByte;
  uint32_t endByte;
  uint32_t namedChildCount;
  uint32_t childCount;
} Node;

void log_to_stdout(void *payload, TSLogType type, const char *message) {
  printf("%s\n", message);
}

void ts_document_log_to_stderr(TSDocument *document) {
  ts_document_set_logger(document, (TSLogger) {.log = log_to_stdout, .payload = NULL});
}

void ts_document_parse_halt_on_error(TSDocument *document) {
  ts_document_parse_with_options(document, (TSParseOptions){
    .changed_ranges = NULL,
    .changed_range_count = NULL,
    .halt_on_error = true
  });
}

static inline Node ts_node_elaborate(const TSDocument *document, TSNode node) {
  return (Node){
    .node = node,
    .symbol = ts_node_symbol(node),
    .type = document ? ts_node_type(node, document) : NULL,
    .startPoint = ts_node_start_point(node),
    .endPoint = ts_node_end_point(node),
    .startByte = ts_node_start_byte(node),
    .endByte = ts_node_end_byte(node),
    .namedChildCount = ts_node_named_child_count(node),
    .childCount = ts_node_child_count(node)
  };
}

void ts_document_root_node_p(TSDocument *document, Node *outNode) {
  assert(document != NULL);
  assert(outNode != NULL);
  TSNode root = ts_document_root_node(document);
  assert(root.data != NULL);
  *outNode = ts_node_elaborate(document, root);
}


void ts_node_copy_child_nodes(const TSDocument *document, const TSNode *parentNode, Node *outChildNodes, size_t count) {
  assert(parentNode != NULL);
  assert(outChildNodes != NULL);
  assert(count >= 0);
  uint32_t maxCount = ts_node_child_count(*parentNode);
  uint32_t max = maxCount <= count ? maxCount : count;
  for (uint32_t i = 0; i < max; i++) {
    outChildNodes[i] = ts_node_elaborate(document, ts_node_child(*parentNode, i));
  }
}

void ts_node_copy_named_child_nodes(const TSDocument *document, const TSNode *parentNode, Node *outChildNodes, size_t count) {
  assert(parentNode != NULL);
  assert(outChildNodes != NULL);
  assert(count >= 0);
  uint32_t maxCount = ts_node_named_child_count(*parentNode);
  uint32_t max = maxCount <= count ? maxCount : count;
  for (uint32_t i = 0; i < max; i++) {
    outChildNodes[i] = ts_node_elaborate(document, ts_node_named_child(*parentNode, i));
  }
}

size_t sizeof_tsnode() {
  return sizeof(TSNode);
}

size_t sizeof_tspoint() {
  return sizeof(TSPoint);
}

size_t sizeof_node() {
  return sizeof(Node);
}
