$Root = Resolve-Path "$PSScriptRoot/.."
$TestSuite = "$Root/../yaml-test-suite";

function GenerateTests($Dir) {
    Get-ChildItem "$Dir/**/in.yaml" -Recurse -Exclude tags | ForEach-Object {
        $Parent = Split-Path -Parent $_
        $Path = ((Resolve-Path $Parent -Relative -RelativeBasePath $Root) -replace '[/\\]','/').Trim('.').Trim('/')
        $Name = "case_" + ((Resolve-Path $Parent -Relative -RelativeBasePath $Dir) -replace '[/\\]','_').Trim('.').Trim('_').ToLower()

        if (Test-Path (Join-Path (Split-Path -Parent $_.FullName) "error")) {
            $FailArg = ", fail: true"
        } else {
            $FailArg = ""
        }

        "case!($Name, `"$Path`"$FailArg);"
    }
}

(. {
    GenerateTests "$Root/yaml-test-suite"
    GenerateTests "$Root/tests/suite/data"
}) | Set-Content "$PSScriptRoot/suite/cases.gen.rs"