import XCTest
import SwiftTreeSitter
import TreeSitterHaskell

final class TreeSitterHaskellTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_haskell())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Haskell grammar")
    }
}
