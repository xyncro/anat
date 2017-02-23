#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

dotnet clean
dotnet restore
dotnet build -c Release
dotnet test -c Release tests/Anat.Tests/Anat.Tests.fsproj
dotnet pack -c Release --include-symbols --include-source --version-suffix "dev"
