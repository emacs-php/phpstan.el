<?php

/*
This file is derived from SanderRonde/phpstan-vscode.
https://github.com/SanderRonde/phpstan-vscode/blob/v4.0.12/php/TreeFetcher.php

Copyright 2022 Sander Ronde (awsdfgvhbjn@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

use PhpParser\Node;
use PhpParser\Node\Expr\ArrowFunction;
use PhpParser\Node\Expr\Assign;
use PhpParser\Node\Expr\ClassConstFetch;
use PhpParser\Node\Expr\Closure;
use PhpParser\Node\Expr\ConstFetch;
use PhpParser\Node\Expr\FuncCall;
use PhpParser\Node\Expr\MethodCall;
use PhpParser\Node\Expr\PropertyFetch;
use PhpParser\Node\Expr\StaticCall;
use PhpParser\Node\Expr\Variable;
use PhpParser\Node\Expr\YieldFrom;
use PhpParser\Node\Expr\Yield_;
use PhpParser\Node\FunctionLike;
use PhpParser\Node\Identifier;
use PhpParser\Node\Name;
use PhpParser\Node\Param;
use PhpParser\Node\Stmt\Return_;
use PHPStan\Analyser\Scope;
use PHPStan\Collectors\Collector;
use PHPStan\Node\CollectedDataNode;
use PHPStan\Node\InForeachNode;
use PHPStan\PhpDocParser\Printer\Printer;
use PHPStan\Reflection\ParametersAcceptor;
use PHPStan\Reflection\Php\PhpMethodFromParserNodeReflection;
use PHPStan\Rules\Rule;
use PHPStan\Type\ArrayType;
use PHPStan\Type\ErrorType;
use PHPStan\Type\Type;
use PHPStan\Type\VerbosityLevel;

class PHPStanEmacsHoverTreeFetcher implements Rule {
	public const REPORTER_FILE = '__PHPSTAN_EMACS_HOVER_REPORT_FILE__';

	public function getNodeType(): string {
		return CollectedDataNode::class;
	}

	public static function binarySearch(&$array, $target) {
		$left = 0;
		$right = count($array) - 1;

		while ($left <= $right) {
			$mid = floor(($left + $right) / 2);

			if ($array[$mid] <= $target) {
				if ($mid == count($array) - 1 || $array[$mid + 1] > $target) {
					return $mid;
				}
				$left = $mid + 1;
			} else {
				$right = $mid - 1;
			}
		}

		return -1;
	}

	/**
	 * @param array<string, list<list<array{
	 *   typeDescr: string,
	 *   phpDocType?: string,
	 *   name: string,
	 *   kind?: string,
	 *   pos: array{
	 *     start: int,
	 *     end: int
	 *   }
	 * }>>> $fileDatas
	 * @return array<string, list<array{
	 *  typeDescr: string,
	 *  phpDocType?: string,
	 *  name: string,
	 *  kind?: string,
	 *  pos: array{
	 *    start: array{line: int, char: int},
	 *    end: array{line: int, char: int}
	 *  }
	 * }>>
	 */
	public static function convertCharIndicesToPositions(array $fileDatas): array {
		$results = [];
		foreach ($fileDatas as $filePath => $fileData) {
			if (count($fileData) === 0) {
				continue;
			}
			$file = file_get_contents($filePath);
			if (!is_string($file)) {
				continue;
			}
			$results[$filePath] = [];

			$lineOffsets = [0];
			for ($i = 0; $i < strlen($file); $i++) {
				if ($file[$i] === "\n") {
					$lineOffsets[] = $i + 1;
				}
			}

			$findPos = static function (int $filePos) use ($lineOffsets) {
				$line = self::binarySearch($lineOffsets, $filePos);
				$lineStart = $lineOffsets[$line];
				$char = $filePos - $lineStart;
				return [
					'line' => $line,
					'char' => $char,
				];
			};

			foreach ($fileData as $nodeData) {
				foreach ($nodeData as $datum) {
					$endPos = $findPos($datum['pos']['end']);
					$entry = [
						'typeDescr' => $datum['typeDescr'],
						'name' => $datum['name'],
						'kind' => $datum['kind'] ?? null,
						'pos' => [
							'start' => $findPos($datum['pos']['start']),
							'end' => [
								'line' => $endPos['line'],
								'char' => $endPos['char'],
							],
						],
					];
					if (isset($datum['phpDocType']) && is_string($datum['phpDocType'])) {
						$entry['phpDocType'] = $datum['phpDocType'];
					}
					$results[$filePath][] = $entry;
				}
			}
		}

		return $results;
	}

	/** @param CollectedDataNode $node */
	public function processNode(Node $node, Scope $scope): array {
		$collectedData = $node->get(PHPStanEmacsHoverTreeFetcherCollector::class);
		file_put_contents(self::REPORTER_FILE, json_encode(self::convertCharIndicesToPositions($collectedData)));
		return [];
	}
}

/**
 * @phpstan-type CollectedData array{
 *   typeDescr: string,
 *   name: string,
 *   pos: array{start: int, end: int}
 * }
 * @implements Collector<Node, list<CollectedData>>
 */
class PHPStanEmacsHoverTreeFetcherCollector implements Collector {
	/** @var ?Printer */
	private $phpDocPrinter = null;

	/** @return array{typeDescr: string, phpDocType?: string} */
	private function describeTypes(Type $type): array {
		$typeDescr = $type->describe(VerbosityLevel::precise());
		$result = ['typeDescr' => $typeDescr];

		$phpDocType = null;
		try {
			$phpDocType = $this->getPhpDocPrinter()->print($type->toPhpDocNode());
		} catch (Throwable $e) {
			$phpDocType = null;
		}

		if ($phpDocType !== null && $phpDocType !== '' && $phpDocType !== $typeDescr) {
			$result['phpDocType'] = $phpDocType;
		}

		return $result;
	}

	private function getPhpDocPrinter(): Printer {
		if ($this->phpDocPrinter === null) {
			$this->phpDocPrinter = new Printer();
		}
		return $this->phpDocPrinter;
	}

	/** @var list<array{ClosureType, list<array{startPos: int, endPos: int, isUsed: false, closureNode: Closure|ArrowFunction}>}> */
	private $closureTypeToNode = [];

	/**
	 * @return ?list<array{startPos: int, endPos: int, isUsed: false, closureNode: Closure|ArrowFunction}>
	 */
	protected function getClosuresFromScope(Scope $scope): ?array {
		$anonymousFunctionReflection = $scope->getAnonymousFunctionReflection();
		if ($anonymousFunctionReflection) {
			foreach ($this->closureTypeToNode as $closureTypeToNode) {
				list($closureType, $closureClosures) = $closureTypeToNode;
				if ($anonymousFunctionReflection !== $closureType) {
					continue;
				}
				return $closureClosures;
			}
		}
		return null;
	}

	protected function processClosures(Node $node, Scope $scope): void {
		if ($node instanceof Closure || $node instanceof ArrowFunction) {
			$closureType = $scope->getType($node);
			$existingClosures = $this->getClosuresFromScope($scope) ?? [];
			$existingClosures[] = [
				'startPos' => $node->getStartFilePos(),
				'endPos' => $node->getEndFilePos() + 1,
				'isUsed' => false,
				'closureNode' => $node,
			];
			$this->closureTypeToNode[] = [$closureType, $existingClosures];
		}
	}

	/** @var list<string> */
	private $visitedFunctions = [];

	/** @return list<CollectedData> */
	private function processFunctionScope(Scope $scope): array {
		$functionKey = implode('.', [
			$scope->getFile(),
			$scope->getClassReflection() ? $scope->getClassReflection()->getName() : null,
			$scope->getFunctionName(),
		]);
		if (in_array($functionKey, $this->visitedFunctions, true)) {
			return [];
		}
		$this->visitedFunctions[] = $functionKey;

		$function = $scope->getFunction();
		assert($function !== null);
		if (!($function instanceof PhpMethodFromParserNodeReflection)) {
			return [];
		}

		$reflectionClass = new ReflectionClass(PhpMethodFromParserNodeReflection::class);
		$reflectionMethod = $reflectionClass->getMethod('getFunctionLike');
		$reflectionMethod->setAccessible(true);
		$fnLike = $reflectionMethod->invoke($function);
		return $this->onFunction($fnLike, $function);
	}

	/** @var list<string> */
	private $visitedClosures = [];

	/**
	 * @param list<array{startPos: int, endPos: int, isUsed: false, closureNode: Closure|ArrowFunction}> $closures
	 * @return list<CollectedData>
	 */
	private function processClosureScope(Scope $scope, array $closures): array {
		$functionKey = implode('.', [
			$scope->getFile(),
			$scope->getClassReflection() ? $scope->getClassReflection()->getName() : null,
			json_encode($closures),
		]);
		if (in_array($functionKey, $this->visitedClosures, true)) {
			return [];
		}
		$this->visitedClosures[] = $functionKey;

		$lastClosure = end($closures);
		/** @var Closure|ArrowFunction $lastClosureNode */
		$lastClosureNode = $lastClosure['closureNode'];
		$fnReflection = $scope->getAnonymousFunctionReflection();
		assert($fnReflection !== null);
		return $this->onClosure($lastClosureNode, $fnReflection);
	}

	/** @return list<CollectedData> */
	public function processFunctionTrackings(Node $node, Scope $scope): array {
		/** @var list<CollectedData> $data */
		$data = [];
		$this->processClosures($node, $scope);

		if ($scope->getFunctionName()) {
			$data = array_merge($data, $this->processFunctionScope($scope));
		}

		$closures = $this->getClosuresFromScope($scope);
		if ($closures) {
			$data = array_merge($data, $this->processClosureScope($scope, $closures));
		}

		return $data;
	}

	public function getNodeType(): string {
		return Node::class;
	}

	/** @return ?CollectedData */
	private function processNodeWithType($node, Type $type): ?array {
		$varName = $node instanceof Variable ? $node->name : $node->name->name;
		if (!is_string($varName)) {
			return null;
		}

		if ($node->getStartFilePos() === -1 || $node->getEndFilePos() === -1) {
			return null;
		}

		return array_merge($this->describeTypes($type), [
			'name' => $varName,
			'kind' => 'variable',
			'pos' => [
				'start' => $node->getStartFilePos() - ($node instanceof Variable ? 1 : 0),
				'end' => $node->getEndFilePos() + 1,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processReturnNode(Return_ $node, Scope $scope): ?array {
		$type = null;
		if ($node->expr !== null) {
			$type = $scope->getType($node->expr);
		} else {
			$function = $scope->getFunction();
			if ($function) {
				$type = $function->getReturnType();
			}
		}

		if (!$type || $type instanceof ErrorType) {
			return null;
		}
		if ($node->getStartFilePos() === -1) {
			return null;
		}

		$start = $node->getStartFilePos();
		$end = $start + 6; // "return"
		if ($node->getEndFilePos() !== -1) {
			$end = min($end, $node->getEndFilePos() + 1);
		}

		return array_merge($this->describeTypes($type), [
			'name' => 'return',
			'kind' => 'return',
			'pos' => [
				'start' => $start,
				'end' => $end,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processYieldNode(Yield_ $node, Scope $scope): ?array {
		$type = null;
		if ($node->value !== null) {
			$type = $scope->getType($node->value);
		}
		if (!$type) {
			$type = $scope->getType($node);
		}
		if (!$type || $type instanceof ErrorType) {
			return null;
		}
		if ($node->getStartFilePos() === -1) {
			return null;
		}

		$start = $node->getStartFilePos();
		$end = $start + 5; // "yield"
		if ($node->getEndFilePos() !== -1) {
			$end = min($end, $node->getEndFilePos() + 1);
		}

		return array_merge($this->describeTypes($type), [
			'name' => 'yield',
			'kind' => 'yield',
			'pos' => [
				'start' => $start,
				'end' => $end,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processYieldFromNode(YieldFrom $node, Scope $scope): ?array {
		$type = $scope->getType($node->expr);
		if (!$type || $type instanceof ErrorType) {
			return null;
		}
		if ($node->getStartFilePos() === -1) {
			return null;
		}

		$start = $node->getStartFilePos();
		$end = $start + 10; // "yield from"
		if ($node->getEndFilePos() !== -1) {
			$end = min($end, $node->getEndFilePos() + 1);
		}

		return array_merge($this->describeTypes($type), [
			'name' => 'yield from',
			'kind' => 'yield-from',
			'pos' => [
				'start' => $start,
				'end' => $end,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processCallNode(Node $nameNode, Type $type, string $name): ?array {
		if ($type instanceof ErrorType) {
			return null;
		}
		if ($nameNode->getStartFilePos() === -1 || $nameNode->getEndFilePos() === -1) {
			return null;
		}

		return array_merge($this->describeTypes($type), [
			'name' => $name,
			'kind' => 'call',
			'pos' => [
				'start' => $nameNode->getStartFilePos(),
				'end' => $nameNode->getEndFilePos() + 1,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processAssignNode(Assign $node, Scope $scope): ?array {
		$assigned = $node->var;
		if (!($assigned instanceof Variable || $assigned instanceof PropertyFetch)) {
			return null;
		}

		$type = $scope->getType($node->expr);
		if ($type instanceof ErrorType) {
			return null;
		}

		return $this->processNodeWithType($assigned, $type);
	}

	/** @return ?CollectedData */
	private function processConstFetchNode(ConstFetch $node, Scope $scope): ?array {
		$type = $scope->getType($node);
		if ($type instanceof ErrorType) {
			return null;
		}
		if ($node->name->getStartFilePos() === -1 || $node->name->getEndFilePos() === -1) {
			return null;
		}

		return array_merge($this->describeTypes($type), [
			'name' => $node->name->toString(),
			'kind' => 'const',
			'pos' => [
				'start' => $node->name->getStartFilePos(),
				'end' => $node->name->getEndFilePos() + 1,
			],
		]);
	}

	/** @return ?CollectedData */
	private function processClassConstFetchNode(ClassConstFetch $node, Scope $scope): ?array {
		if (!($node->name instanceof Identifier)) {
			return null;
		}

		$type = $scope->getType($node);
		if ($type instanceof ErrorType) {
			return null;
		}
		if ($node->name->getStartFilePos() === -1 || $node->name->getEndFilePos() === -1) {
			return null;
		}

		$className = ($node->class instanceof Name) ? $node->class->toString() : null;
		$constName = $node->name->toString();
		$displayName = $className ? ($className . '::' . $constName) : $constName;

		return array_merge($this->describeTypes($type), [
			'name' => $displayName,
			'kind' => 'class-const',
			'pos' => [
				'start' => $node->name->getStartFilePos(),
				'end' => $node->name->getEndFilePos() + 1,
			],
		]);
	}

	/** @return list<CollectedData> */
	protected function onClosure($node, ParametersAcceptor $type): array {
		/** @var array<string, Param> */
		$paramNodesByName = [];
		foreach ($node->getParams() as $param) {
			$paramNodesByName[$param->var->name] = $param;
		}

		/** @var list<CollectedData> */
		$data = [];
		foreach ($type->getParameters() as $parameter) {
			$paramNode = $paramNodesByName[$parameter->getName()] ?? null;
			if (!$paramNode) {
				continue;
			}

			$typeData = $this->describeTypes($parameter->getType());
			if ($paramNode->getStartFilePos() === -1 || $paramNode->getEndFilePos() === -1) {
				continue;
			}

			$data[] = array_merge($typeData, [
				'name' => $parameter->getName(),
				'pos' => [
					'start' => $paramNode->getStartFilePos(),
					'end' => $paramNode->getEndFilePos() + 1,
				],
			]);
		}

		return $data;
	}

	/** @return list<CollectedData> */
	protected function onFunction(FunctionLike $node, PhpMethodFromParserNodeReflection $type): array {
		/** @var list<CollectedData> $data */
		$data = [];

		/** @var array<string, Param> */
		$paramNodesByName = [];
		foreach ($node->getParams() as $param) {
			$paramNodesByName[$param->var->name] = $param;
		}

		foreach ($type->getVariants() as $variant) {
			foreach ($variant->getParameters() as $parameter) {
				$paramNode = $paramNodesByName[$parameter->getName()] ?? null;
				if (!$paramNode) {
					continue;
				}

				$typeData = $this->describeTypes($parameter->getType());
				if ($paramNode->getStartFilePos() === -1 || $paramNode->getEndFilePos() === -1) {
					continue;
				}

				$data[] = array_merge($typeData, [
					'name' => $parameter->getName(),
					'pos' => [
						'start' => $paramNode->getStartFilePos(),
						'end' => $paramNode->getEndFilePos() + 1,
					],
				]);
			}
		}

		return $data;
	}

	/** @return ?list<CollectedData> */
	public function processNode(Node $node, Scope $scope): ?array {
		if ($scope->getTraitReflection()) {
			return null;
		}

		/** @var list<CollectedData> $data */
		$data = [];
		$data = array_merge($data, $this->processFunctionTrackings($node, $scope));

		if ($node instanceof InForeachNode) {
			$keyVar = $node->getOriginalNode()->keyVar;
			$valueVar = $node->getOriginalNode()->valueVar;
			$exprType = $scope->getType($node->getOriginalNode()->expr);
			if ($exprType instanceof ArrayType) {
				if ($keyVar && $keyVar instanceof Variable) {
					$nodeWithType = $this->processNodeWithType($keyVar, $exprType->getKeyType());
					if ($nodeWithType) {
						$data[] = $nodeWithType;
					}
				}
				if ($valueVar && $valueVar instanceof Variable) {
					$nodeWithType = $this->processNodeWithType($valueVar, $exprType->getItemType());
					if ($nodeWithType) {
						$data[] = $nodeWithType;
					}
				}
			}
		}

		if ($node instanceof Assign) {
			$assignNodeData = $this->processAssignNode($node, $scope);
			if ($assignNodeData) {
				$data[] = $assignNodeData;
			}
		}

		if ($node instanceof ConstFetch) {
			$constNodeData = $this->processConstFetchNode($node, $scope);
			if ($constNodeData) {
				$data[] = $constNodeData;
			}
		}

		if ($node instanceof ClassConstFetch) {
			$classConstNodeData = $this->processClassConstFetchNode($node, $scope);
			if ($classConstNodeData) {
				$data[] = $classConstNodeData;
			}
		}

		if ($node instanceof FuncCall && $node->name instanceof Name) {
			$callNodeData = $this->processCallNode($node->name, $scope->getType($node), $node->name->toString());
			if ($callNodeData) {
				$data[] = $callNodeData;
			}
		}

		if ($node instanceof MethodCall && $node->name instanceof Identifier) {
			$callNodeData = $this->processCallNode($node->name, $scope->getType($node), $node->name->toString());
			if ($callNodeData) {
				$data[] = $callNodeData;
			}
		}

		if ($node instanceof StaticCall && $node->name instanceof Identifier) {
			$callNodeData = $this->processCallNode($node->name, $scope->getType($node), $node->name->toString());
			if ($callNodeData) {
				$data[] = $callNodeData;
			}
		}

		if ($node instanceof Variable || $node instanceof PropertyFetch) {
			$type = $scope->getType($node);
			$parent = $node->getAttribute('parent');
			if ($parent && $parent instanceof Assign) {
				$type = $scope->getType($parent->expr);
			}
			if (!($type instanceof ErrorType)) {
				$nodeWithType = $this->processNodeWithType($node, $type);
				if ($nodeWithType) {
					$data[] = $nodeWithType;
				}
			}
		}

		if ($node instanceof Return_) {
			$returnNodeData = $this->processReturnNode($node, $scope);
			if ($returnNodeData) {
				$data[] = $returnNodeData;
			}
		}

		if ($node instanceof Yield_) {
			$yieldNodeData = $this->processYieldNode($node, $scope);
			if ($yieldNodeData) {
				$data[] = $yieldNodeData;
			}
		}

		if ($node instanceof YieldFrom) {
			$yieldFromNodeData = $this->processYieldFromNode($node, $scope);
			if ($yieldFromNodeData) {
				$data[] = $yieldFromNodeData;
			}
		}

		if ($data === []) {
			return null;
		}
		return $data;
	}
}
