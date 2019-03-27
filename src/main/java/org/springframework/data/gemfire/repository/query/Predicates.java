/*
 * Copyright 2012-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.repository.query;

import java.util.Iterator;

import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;
import org.springframework.util.Assert;

class Predicates implements Predicate {

	private final Predicate current;

	/**
	 * Creates a new {@link Predicates} wrapper instance.
	 *
	 * @param predicate must not be {@literal null}.
	 */
	private Predicates(Predicate predicate) {
		this.current = predicate;
	}

	private static Predicates create(Predicate predicate) {
		return new Predicates(predicate);
	}

	/**
	 * Creates a new Predicate for the given {@link Part} and index iterator.
	 *
	 * @param part must not be {@literal null}.
	 * @param indexes must not be {@literal null}.
	 * @return an instance of {@link Predicates} wrapping the WHERE clause condition expression ({@link Part}).
	 */
	public static Predicates create(Part part, Iterator<Integer> indexes) {
		return create(new AtomicPredicate(part, indexes));
	}

	/**
	 * And-concatenates the given {@link Predicate} to the current one.
	 *
	 * @param predicate must not be {@literal null}.
	 * @return an instance of {@link Predicates} wrapping an AND condition.
	 */
	public Predicates and(final Predicate predicate) {
		return create(new Predicate() {
			@Override
			public String toString(String alias) {
				return String.format("%s AND %s", Predicates.this.current.toString(alias), predicate.toString(alias));
			}
		});
	}

	/**
	 * Or-concatenates the given {@link Predicate} to the current one.
	 *
	 * @param predicate must not be {@literal null}.
	 * @return an instance of {@link Predicates} wrapping an OR condition.
	 */
	public Predicates or(final Predicate predicate) {
		return create(new Predicate() {
			@Override
			public String toString(String alias) {
				return String.format("%s OR %s", Predicates.this.current.toString(alias), predicate.toString(alias));
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.repository.query.Predicate#toString(java.lang.String)
	 */
	@Override
	public String toString(String alias) {
		return current.toString(alias);
	}

	/**
	 * Predicate to create a predicate expression for a {@link Part}.
	 *
	 * @author Oliver Gierke
	 */
	public static class AtomicPredicate implements Predicate {

		private final Iterator<Integer> indexes;

		private final Part part;

		/**
		 * Creates a new {@link AtomicPredicate}.
		 *
		 * @param part must not be {@literal null}.
		 * @param indexes must not be {@literal null}.
		 */
		public AtomicPredicate(Part part, Iterator<Integer> indexes) {
			Assert.notNull(part, "Query Predicate Part must not be null");
			Assert.notNull(indexes, "Iterator of numeric, indexed query parameter placeholders must not be null");

			this.part = part;
			this.indexes = indexes;
		}

		/**
		 * Builds a conditional expression for the entity property in the WHERE clause of the GemFire OQL
		 * query statement.
		 *
		 * @see org.springframework.data.gemfire.repository.query.Predicate#toString(java.lang.String)
		 */
		@Override
		public String toString(String alias) {
			if (isIgnoreCase()) {
				return String.format("%s.equalsIgnoreCase($%d)", resolveProperty(alias), indexes.next());
			}
			else {
				Type partType = part.getType();

				switch (partType) {
					case IS_NULL:
					case IS_NOT_NULL:
						return String.format("%s %s NULL", resolveProperty(alias), resolveOperator(partType));
					case FALSE:
					case TRUE:
						return String.format("%s %s %s", resolveProperty(alias), resolveOperator(partType),
							Type.TRUE.equals(partType));
					default:
						return String.format("%s %s $%d", resolveProperty(alias), resolveOperator(partType),
							indexes.next());
				}
			}
		}

		boolean isIgnoreCase() {
			switch (part.shouldIgnoreCase()) {
				case ALWAYS:
				case WHEN_POSSIBLE:
					return true;
				case NEVER:
				default:
					return false;
			}
		}

		String resolveProperty(String alias) {
			return String.format("%1$s.%2$s", resolveAlias(alias), part.getProperty().toDotPath());
		}

		String resolveAlias(String alias) {
			return (alias != null ? alias : QueryBuilder.DEFAULT_ALIAS);
		}

		/**
		 * Resolves the given {@link Type} as an GemFire OQL operator.
		 *
		 * @param partType the conditional expression (e.g. 'IN') in the query method name.
		 * @return a GemFire OQL operator.
		 */
		String resolveOperator(Type partType) {
			switch (partType) {
				// Equality - Is
				case FALSE:
				case IS_NULL:
				case SIMPLE_PROPERTY:
				case TRUE:
					return "=";
				// Equality - Is Not
				case IS_NOT_NULL:
				case NEGATING_SIMPLE_PROPERTY:
					return "!=";
				// Relational Comparison
				case GREATER_THAN:
					return ">";
				case GREATER_THAN_EQUAL:
					return ">=";
				case LESS_THAN:
					return "<";
				case LESS_THAN_EQUAL:
					return "<=";
				// Set Containment
				case IN:
					return "IN SET";
				case NOT_IN:
					return "NOT IN SET";
				// Wildcard Matching
				case LIKE:
				case STARTING_WITH:
				case ENDING_WITH:
				case CONTAINING:
					return "LIKE";
				default:
					throw new IllegalArgumentException(String.format("Unsupported operator %s!", partType));
			}
		}
	}
}
