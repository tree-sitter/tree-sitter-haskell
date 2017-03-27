#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>

typedef struct Node {
  TSNode node;
  const char *type;
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

void ts_document_root_node_p(TSDocument *document, Node *outNode) {
	assert(document != NULL);
	assert(outNode != NULL);
	TSNode root = ts_document_root_node(document);
	assert(root.data != NULL);
  *outNode = (Node){
    .node = root,
    .type = ts_node_type(root, document),
    .startPoint = ts_node_start_point(root),
    .endPoint = ts_node_end_point(root),
    .startByte = ts_node_start_byte(root),
    .endByte = ts_node_end_byte(root),
    .namedChildCount = ts_node_named_child_count(root),
    .childCount = ts_node_child_count(root)
  };
}

const char *ts_node_p_name(const TSNode *node, const TSDocument *document) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(document != NULL);
	return ts_node_type(*node, document);
}

size_t ts_node_p_child_count(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_child_count(*node);
}

size_t ts_node_p_named_child_count(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_named_child_count(*node);
}

void ts_node_p_child(const TSNode *node, size_t index, TSNode *outNode) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(outNode != NULL);
	TSNode temp = ts_node_child(*node, index);
	if (temp.data == NULL) {
		printf("got broken child for index %ld\n", index);
	}
	assert(temp.data != NULL);
	*outNode = temp;
}

void ts_node_p_named_child(const TSNode *node, size_t index, TSNode *outNode) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(outNode != NULL);
	TSNode temp = ts_node_named_child(*node, index);
	if (temp.data == NULL) {
		printf("got broken child for index %ld\n", index);
	}
	assert(temp.data != NULL);
	*outNode = temp;
}


size_t ts_node_p_start_char(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_char(*node);
}

size_t ts_node_p_end_char(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_char(*node);
}


size_t ts_node_p_start_byte(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_byte(*node);
}

size_t ts_node_p_end_byte(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_byte(*node);
}


size_t ts_node_p_start_point_row(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_point(*node).row;
}

size_t ts_node_p_start_point_column(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_point(*node).column;
}

size_t ts_node_p_end_point_row(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_point(*node).row;
}

size_t ts_node_p_end_point_column(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_point(*node).column;
}
