/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
	 * @param value must not be {@literal null}.
	 * @return
	 */
	public static Predicates create(Part part, Iterator<Integer> value) {
		return create(new AtomicPredicate(part, value));
	}

	/**
	 * And-concatenates the given {@link Predicate} to the current one.
	 * 
	 * @param predicate must not be {@literal null}.
	 * @return
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
	 * @return
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

		private final Part part;
		private final Iterator<Integer> value;

		/**
		 * Creates a new {@link AtomicPredicate}.
		 * 
		 * @param part must not be {@literal null}.
		 * @param value must not be {@literal null}.
		 */
		public AtomicPredicate(Part part, Iterator<Integer> value) {

			Assert.notNull(part);
			Assert.notNull(value);

			this.part = part;
			this.value = value;
		}

		/*
		 * (non-Javadoc)
		 * @see org.springframework.data.gemfire.repository.query.Predicate#toString(java.lang.String)
		 */
		@Override
		public String toString(String alias) {

			Type type = part.getType();
			return String.format("%s.%s %s", alias == null ? QueryBuilder.DEFAULT_ALIAS : alias, part.getProperty()
					.toDotPath(), toClause(type));
		}

		private String toClause(Type type) {

			switch (type) {
			case IS_NULL:
			case IS_NOT_NULL:
				return String.format("%s NULL", getOperator(type));
			default:
				return String.format("%s $%s", getOperator(type), value.next());
			}
		}

		/**
		 * Maps the given {@link Type} to an OQL operator.
		 * 
		 * @param type
		 * @return
		 */
		private String getOperator(Type type) {

			switch (type) {
			case IN:
				return "IN SET";
			case NOT_IN:
				return "NOT IN SET";
			case GREATER_THAN:
				return ">";
			case GREATER_THAN_EQUAL:
				return ">=";
			case LESS_THAN:
				return "<";
			case LESS_THAN_EQUAL:
				return "<=";
			case IS_NOT_NULL:
			case NEGATING_SIMPLE_PROPERTY:
				return "!=";
			case IS_NULL:
			case SIMPLE_PROPERTY:
				return "=";
			default:
				throw new IllegalArgumentException(String.format("Unsupported operator %s!", type));
			}
		}
	}
}