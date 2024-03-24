package tree_sitter_haskell_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-haskell"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_haskell.Language())
	if language == nil {
		t.Errorf("Error loading Haskell grammar")
	}
}
