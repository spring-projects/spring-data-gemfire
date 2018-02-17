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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.repository.query.parser.AbstractQueryCreator;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * {@link AbstractQueryCreator} to create {@link QueryString} instances.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.query.QueryBuilder
 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator
 * @see org.springframework.data.repository.query.parser.Part
 * @see org.springframework.data.repository.query.parser.PartTree
 */
class GemfireQueryCreator extends AbstractQueryCreator<QueryString, Predicates> {

	private static final Log LOG = LogFactory.getLog(GemfireQueryCreator.class);

	private Iterator<Integer> indexes;

	private final QueryBuilder queryBuilder;

	/**
	 * Creates a new {@link GemfireQueryCreator} using the given {@link PartTree} and domain class.
	 *
	 * @param tree must not be {@literal null}.
	 * @param entity must not be {@literal null}.
	 */
	public GemfireQueryCreator(PartTree tree, GemfirePersistentEntity<?> entity) {

		super(tree);

		this.queryBuilder = new QueryBuilder(entity, tree);
		this.indexes = new IndexProvider();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator#createQuery(org.springframework.data.domain.Sort)
	 */
	@Override
	public QueryString createQuery(Sort dynamicSort) {

		this.indexes = new IndexProvider();

		return super.createQuery(dynamicSort);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator#create(org.springframework.data.repository.query.parser.Part, java.util.Iterator)
	 */
	@Override
	protected Predicates create(Part part, Iterator<Object> iterator) {
		return Predicates.create(part, this.indexes);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator#and(org.springframework.data.repository.query.parser.Part, java.lang.Object, java.util.Iterator)
	 */
	@Override
	protected Predicates and(Part part, Predicates base, Iterator<Object> iterator) {
		return base.and(Predicates.create(part, this.indexes));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator#or(java.lang.Object, java.lang.Object)
	 */
	@Override
	protected Predicates or(Predicates base, Predicates criteria) {
		return base.or(criteria);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.parser.AbstractQueryCreator#complete(java.lang.Object, org.springframework.data.domain.Sort)
	 */
	@Override
	protected QueryString complete(Predicates criteria, Sort sort) {

		QueryString query = this.queryBuilder.create(criteria).orderBy(sort);

		if (LOG.isDebugEnabled()) {
			LOG.debug(String.format("Created Query [%s]", query.toString()));
		}

		return query;
	}

	/**
	 * {@link IndexProvider} is an {@link Iterator} providing sequentially numbered placeholders (starting at 1),
	 * in a generated GemFire OQL statement corresponding to all possible arguments passed to
	 * the query's indexed parameters.
	 *
	 * @see java.util.Iterator
	 */
	private static class IndexProvider implements Iterator<Integer> {

		private int index;

		public IndexProvider() {
			this.index = 1;
		}

		/*
		 * (non-Javadoc)
		 * @see java.util.Iterator#hasNext()
		 */
		@Override
		@SuppressWarnings("all")
		public boolean hasNext() {
			return (index <= Integer.MAX_VALUE);
		}

		/*
		 * (non-Javadoc)
		 * @see java.util.Iterator#next()
		 */
		@Override
		public Integer next() {
			return index++;
		}

		/*
		 * (non-Javadoc)
		 * @see java.util.Iterator#remove()
		 */
		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
}
