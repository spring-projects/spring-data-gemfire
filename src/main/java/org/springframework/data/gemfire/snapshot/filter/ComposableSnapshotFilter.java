/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.snapshot.filter;

import java.util.Map;

import org.apache.geode.cache.snapshot.SnapshotFilter;

/**
 * The ComposableSnapshotFilter class is a GemFire SnapshotFilter implementation of the Composition design pattern
 * allowing 2 or more SnapshotFilters to be combined by logical AND and OR operators acting as a single SnapshotFilter.
 *
 * @author John Blum
 * @see org.apache.geode.cache.snapshot.SnapshotFilter
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class ComposableSnapshotFilter<K, V> implements SnapshotFilter<K, V> {

	/**
	 * Operator is an enumeration of logical operators (AND, OR) used to compose SnapshotFilters
	 * into a boolean expression.
	 */
	protected enum Operator {
		AND,
		OR;

		public boolean isAnd() {
			return (this == AND);
		}

		public boolean isOr() {
			return (this == OR);
		}

		public boolean operate(boolean leftOperand, boolean rightOperand) {
			return (isAnd() ? (leftOperand && rightOperand) : (leftOperand || rightOperand));
		}
	}

	private final Operator operator;

	private final SnapshotFilter<K, V> leftOperand;
	private final SnapshotFilter<K, V> rightOperand;

	/**
	 * Constructs an instance of ComposableSnapshotFilter initialized with a logical operator combining the resulting
	 * boolean expressions from the evaluation of the operands.
	 *
	 * @param leftOperand the left operand in the boolean-based expression.
	 * @param operator the right operand in the boolean-based expression.
	 * @param rightOperand the operator used to combine the resulting boolean expressions
	 * from the evaluation of the operands.
	 * @see ComposableSnapshotFilter.Operator
	 * @see org.apache.geode.cache.snapshot.SnapshotFilter
	 */
	private ComposableSnapshotFilter(SnapshotFilter<K, V> leftOperand, Operator operator, SnapshotFilter<K, V> rightOperand) {
		this.leftOperand = leftOperand;
		this.operator = operator;
		this.rightOperand = rightOperand;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	static <K, V> SnapshotFilter<K, V>[] nullSafeArray(SnapshotFilter<K, V>... array) {
		return (array != null ? array : new SnapshotFilter[0]);
	}

	/**
	 * Composes the array of SnapshotFilters into a logical boolean expression using the specified Operator.
	 *
	 * @param <K> the class type of the SnapshotFilter key.
	 * @param <V> the class type of the SnapshotFilter value.
	 * @param operator the logical operator used to compose the SnapshotFilters.
	 * @param snapshotFilters the array of SnapshotFilters to compose into a logical boolean expression
	 * using the Operator.
	 * @return a SnapshotFilter implementation composed of the SnapshotFilters using the specified Operator.
	 * @see ComposableSnapshotFilter.Operator
	 * @see org.apache.geode.cache.snapshot.SnapshotFilter
	 */
	protected static <K, V> SnapshotFilter<K, V> compose(Operator operator, SnapshotFilter<K, V>... snapshotFilters) {
		SnapshotFilter<K, V> composedSnapshotFilter = null;

		for (SnapshotFilter<K, V> snapshotFilter : nullSafeArray(snapshotFilters)) {
			composedSnapshotFilter = (composedSnapshotFilter == null ? snapshotFilter
				: new ComposableSnapshotFilter<K, V>(snapshotFilter, operator, composedSnapshotFilter));
		}

		return composedSnapshotFilter;
	}

	/**
	 * Composes the array of SnapshotFilters into a logical boolean expression using the AND Operator.
	 *
	 * @param <K> the class type of the SnapshotFilter key.
	 * @param <V> the class type of the SnapshotFilter value.
	 * @param snapshotFilters the array of SnapshotFilters to compose into a logical boolean expression
	 * using the AND Operator.
	 * @return a SnapshotFilter implementation composed of the SnapshotFilters using the AND Operator.
	 * @see ComposableSnapshotFilter.Operator#AND
	 * @see org.apache.geode.cache.snapshot.SnapshotFilter
	 */
	public static <K, V> SnapshotFilter<K, V> and(SnapshotFilter<K, V>... snapshotFilters) {
		return compose(Operator.AND, snapshotFilters);
	}

	/**
	 * Composes the array of SnapshotFilters into a logical boolean expression using the OR Operator.
	 *
	 * @param <K> the class type of the SnapshotFilter key.
	 * @param <V> the class type of the SnapshotFilter value.
	 * @param snapshotFilters the array of SnapshotFilters to compose into a logical boolean expression
	 * using the OR Operator.
	 * @return a SnapshotFilter implementation composed of the SnapshotFilters using the OR Operator.
	 * @see ComposableSnapshotFilter.Operator#OR
	 * @see org.apache.geode.cache.snapshot.SnapshotFilter
	 */
	public static <K, V> SnapshotFilter<K, V> or(SnapshotFilter<K, V>... snapshotFilters) {
		return compose(Operator.OR, snapshotFilters);
	}

	/**
	 * Determines whether the following Map Entry is accepted by this composed SnapshotFilter implementation.
	 *
	 * @param entry the Map.Entry to evaluate.
	 * @return a boolean value indicating whether this composed SnapshotFilter accepts the Map Entry.
	 * @see ComposableSnapshotFilter.Operator
	 * @see org.apache.geode.cache.snapshot.SnapshotFilter#accept(Map.Entry)
	 * @see java.util.Map.Entry
	 */
	@Override
	public boolean accept(final Map.Entry<K, V> entry) {
		return operator.operate(leftOperand.accept(entry), rightOperand.accept(entry));
	}

}
