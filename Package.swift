// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterHaskell",
    products: [
        .library(name: "TreeSitterHaskell", targets: ["TreeSitterHaskell"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterHaskell",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                "src/scanner.c",
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterHaskellTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterHaskell",
            ],
            path: "bindings/swift/TreeSitterHaskellTests"
        )
    ],
    cLanguageStandard: .c11
)
