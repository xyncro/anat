#!/bin/bash

set -euo pipefail

echo "=== dotnet restore ==="
dotnet restore

echo "=== dotnet build $@ ==="
dotnet build src/Anat -f netstandard1.6 $@

echo "=== dotnet test $@ ==="
dotnet test tests/Anat.Tests -f netcoreapp1.0 $@
