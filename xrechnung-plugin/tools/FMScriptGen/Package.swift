// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "FMScriptGen",
    platforms: [
        .macOS(.v12)
    ],
    products: [
        .executable(
            name: "fmscriptgen",
            targets: ["FMScriptGen"])
    ],
    targets: [
        .executableTarget(
            name: "FMScriptGen",
            path: "Sources")
    ]
)
