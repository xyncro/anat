#!/bin/bash

set -euo pipefail

echo "=== dotnet restore ==="
dotnet restore

echo "=== dotnet build ==="
dotnet build src/Anat -c Release -f netstandard1.6

echo "=== dotnet test ==="
dotnet test tests/Anat.Tests -c Release -f netcoreapp1.0
