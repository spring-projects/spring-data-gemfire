/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.search.lucene.support;

import static org.springframework.data.gemfire.domain.ListablePage.newListablePage;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;

import org.springframework.data.domain.Page;
import org.springframework.data.gemfire.domain.support.AbstractPageSupport;
import org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor;
import org.springframework.util.Assert;

/**
 * The {@link LucenePage} class is a Spring Data {@link Page} implementation supporting Spring Data style paging
 * of {@link PageableLuceneQueryResults} complete with Spring Data projections.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.apache.geode.cache.lucene.LuceneResultStruct
 * @see org.apache.geode.cache.lucene.PageableLuceneQueryResults
 * @see org.springframework.data.domain.Page
 * @see org.springframework.data.gemfire.domain.support.AbstractPageSupport
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor
 * @since 1.1.0
 */
public class LucenePage<T, K, V> extends AbstractPageSupport<T> {

	/**
	 * Factory method used to construct a new instance of {@link LucenePage} initialized with
	 * the given {@link PageableLuceneQueryResults Lucene query results}, {@link Integer page size},
	 * and {@link Class projection type}.
	 *
	 * The {@link LucenePage previous page} is set to {@literal null}.
	 *
	 * @param template {@link ProjectingLuceneAccessor} used to perform Lucene queries and  data access operations
	 * along with projections.
	 * @param queryResults {@link PageableLuceneQueryResults} wrapped by this {@link LucenePage}.
	 * @param pageSize number of elements on a {@link LucenePage}.
	 * @param projectionType {@link Class} type of the projection used to view an individual {@link LuceneResultStruct}
	 * in the {@link PageableLuceneQueryResults Lucene query results}.
	 * @throws IllegalArgumentException if {@link ProjectingLuceneAccessor} or the {@link PageableLuceneQueryResults}
	 * are {@literal null}, or the {@link PageableLuceneQueryResults} do not have
	 * a {@link PageableLuceneQueryResults#hasNext() next page}.
	 * @see #LucenePage(ProjectingLuceneAccessor, PageableLuceneQueryResults, int, Class)
	 */
	public static <T, K, V> LucenePage<T, K, V> newLucenePage(ProjectingLuceneAccessor template,
			PageableLuceneQueryResults<K, V> queryResults, int pageSize, Class<T> projectionType) {

		return new LucenePage<>(template, queryResults, pageSize, projectionType);
	}

	/**
	 * Factory method used to construct a new instance of {@link LucenePage} initialized with
	 * the given {@link PageableLuceneQueryResults Lucene query results}, {@link Integer page size},
	 * {@link Class projection type} and {@link LucenePage previous page}, if one exists.
	 *
	 * @param template {@link ProjectingLuceneAccessor} used to perform Lucene queries and  data access operations
	 * along with projections.
	 * @param queryResults {@link PageableLuceneQueryResults} wrapped by this {@link LucenePage}.
	 * @param pageSize number of elements on a {@link LucenePage}.
	 * @param projectionType {@link Class} type of the projection used to view an individual {@link LuceneResultStruct}
	 * in the {@link PageableLuceneQueryResults Lucene query results}.
	 * @param previousPage {@link LucenePage previous page} in the chain of {@link LucenePage pages},
	 * if this {@link LucenePage} is not the first {@link LucenePage}.  Can be {@literal null}.
	 * @throws IllegalArgumentException if {@link ProjectingLuceneAccessor} or the {@link PageableLuceneQueryResults}
	 * are {@literal null}, or the {@link PageableLuceneQueryResults} do not have
	 * a {@link PageableLuceneQueryResults#hasNext() next page}.
	 * @see #LucenePage(ProjectingLuceneAccessor, PageableLuceneQueryResults, int, Class, LucenePage)
	 */
	public static <T, K, V> LucenePage<T, K, V> newLucenePage(ProjectingLuceneAccessor template,
			PageableLuceneQueryResults<K, V> queryResults, int pageSize, Class<T> projectionType,
			LucenePage<T, K, V> previousPage) {

		return new LucenePage<>(template, queryResults, pageSize, projectionType, previousPage);
	}

	private LucenePage<T, K, V> next;
	private LucenePage<T, K, V> previous;

	private final int pageSize;

	private final Class<T> projectionType;

	private final List<T> content;

	private final PageableLuceneQueryResults<K, V> queryResults;

	private final ProjectingLuceneAccessor template;

	/**
	 * Constructs a new instance of {@link LucenePage} initialized with
	 * the given {@link PageableLuceneQueryResults Lucene query results}, {@link Integer page size}
	 * and {@link Class projection type}.
	 *
	 * The {@link LucenePage previous page} is set to {@literal null}.
	 *
	 * @param template {@link ProjectingLuceneAccessor} used to perform Lucene queries and  data access operations
	 * along with projections.
	 * @param queryResults {@link PageableLuceneQueryResults} wrapped by this {@link LucenePage}.
	 * @param pageSize number of elements on a {@link LucenePage}.
	 * @param projectionType {@link Class} type of the projection used to view an individual {@link LuceneResultStruct}
	 * in the {@link PageableLuceneQueryResults Lucene query results}.
	 * @throws IllegalArgumentException if {@link ProjectingLuceneAccessor} or the {@link PageableLuceneQueryResults}
	 * are {@literal null}, or the {@link PageableLuceneQueryResults} do not have
	 * a {@link PageableLuceneQueryResults#hasNext() next page}.
	 * @see #LucenePage(ProjectingLuceneAccessor, PageableLuceneQueryResults, int, Class, LucenePage)
	 */
	public LucenePage(ProjectingLuceneAccessor template, PageableLuceneQueryResults<K, V> queryResults,
			int pageSize, Class<T> projectionType) {

		this(template, queryResults, pageSize, projectionType, null);
	}

	/**
	 * Constructs a new instance of {@link LucenePage} initialized with
	 * the given {@link PageableLuceneQueryResults Lucene query results}, {@link Integer page size},
	 * {@link Class projection type} and {@link LucenePage previous page}, if one exists.
	 *
	 * @param template {@link ProjectingLuceneAccessor} used to perform Lucene queries and  data access operations
	 * along with projections.
	 * @param queryResults {@link PageableLuceneQueryResults} wrapped by this {@link LucenePage}.
	 * @param pageSize number of elements on a {@link LucenePage}.
	 * @param projectionType {@link Class} type of the projection used to view an individual {@link LuceneResultStruct}
	 * in the {@link PageableLuceneQueryResults Lucene query results}.
	 * @param previous {@link LucenePage previous page} in the chain of {@link LucenePage pages},
	 * if this {@link LucenePage} is not the first {@link LucenePage}.  Can be {@literal null}.
	 * @throws IllegalArgumentException if {@link ProjectingLuceneAccessor} or the {@link PageableLuceneQueryResults}
	 * are {@literal null}, or the {@link PageableLuceneQueryResults} do not have
	 * a {@link PageableLuceneQueryResults#hasNext() next page}.
	 * @see #materialize(ProjectingLuceneAccessor, List, Class)
	 */
	public LucenePage(ProjectingLuceneAccessor template, PageableLuceneQueryResults<K, V> queryResults,
			int pageSize, Class<T> projectionType, LucenePage<T, K, V> previous) {

		Assert.notNull(template, "ProjectingLuceneAccessor must not be null");
		Assert.notNull(queryResults, "PageableLuceneQueryResults must not be null");
		Assert.isTrue(queryResults.hasNext(), "PageableLuceneQueryResults must have content");

		this.template = template;
		this.queryResults = queryResults;
		this.pageSize = pageSize;
		this.projectionType = projectionType;
		this.previous = previous;
		this.content = materialize(template, queryResults.next(), projectionType);
	}

	/**
	 * Renders the {@link List} of {@link LuceneResultStruct} objects into projected values based on
	 * the {@link Class projection type}.
	 *
	 * @param template {@link ProjectingLuceneAccessor} used to project the desired values
	 * from the {@link List} of {@link LuceneResultStruct} objects.
	 * @param pageOfQueryResults Lucene query results captured in the {@link List}
	 * of {@link LuceneResultStruct} objects.
	 * @param projectionType {@link Class} type to project the {@link LuceneResultStruct} objects as.
	 * @return a {@link List} of projected values of the given {@link Class projection type}.
	 * @see org.apache.geode.cache.lucene.LuceneResultStruct
	 */
	protected List<T> materialize(ProjectingLuceneAccessor template, List<LuceneResultStruct<K, V>> pageOfQueryResults,
			Class<T> projectionType) {

		return template.project(pageOfQueryResults, projectionType);
	}

	/**
	 * Returns the number of elements per {@link LucenePage page}.
	 *
	 * @return an integer value indicating the number of elements on a {@link LucenePage page}.
	 */
	protected int getPageSize() {
		return this.pageSize;
	}

	/**
	 * Returns the {@link Class} type of the projection.
	 *
	 * @return a {@link Class} specifying the projection type.
	 */
	protected Class<T> getProjectionType() {
		return this.projectionType;
	}

	/**
	 * Returns the {@link PageableLuceneQueryResults Lucene query results} backing this {@link LucenePage}.
	 *
	 * @return a reference to the {@link PageableLuceneQueryResults} backing this {@link LucenePage}.
	 * @see org.springframework.data.gemfire.search.lucene.support.LucenePage
	 * @see org.apache.geode.cache.lucene.PageableLuceneQueryResults
	 */
	protected PageableLuceneQueryResults<K, V> getQueryResults() {
		return this.queryResults;
	}

	/**
	 * Returns the {@link ProjectingLuceneAccessor} used by this {@link LucenePage} to perform
	 * Lucene data access operations and projections.
	 *
	 * @return the {@link ProjectingLuceneAccessor} used by this {@link LucenePage} to perform
	 * Lucene data access operations and projections.
	 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneAccessor
	 */
	protected ProjectingLuceneAccessor getTemplate() {
		return this.template;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasNext() {
		return getQueryResults().hasNext();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean hasPrevious() {
		return (getPrevious() != null);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public List<T> getContent() {
		return Collections.unmodifiableList(this.content);
	}

	/**
	 * Null-safe method to return the next {@link LucenePage page} in the collection of {@link Page pages}.
	 *
	 * @return the next {@link LucenePage page} in the collection of {@link Page pages}.
	 * @throws IllegalStateException if no more {@link Page pages} exist beyond this {@link LucenePage page}.
	 * @see org.springframework.data.gemfire.search.lucene.support.LucenePage
	 * @see #getPrevious()
	 */
	public LucenePage<T, K, V> getNext() {
		return Optional.ofNullable(this.next).orElseGet(() -> {
			Assert.state(hasNext(), "No more pages");
			this.next = newLucenePage(getTemplate(), getQueryResults(), getPageSize(), getProjectionType(), this);
			return this.next;
		});
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getNumber() {
		AtomicInteger number = new AtomicInteger(1);

		Optional.ofNullable(getPrevious()).ifPresent(previous -> {
			while (previous != null) {
				previous = previous.getPrevious();
				number.incrementAndGet();
			}
		});

		return number.get();
	}

	/**
	 * Returns the previous {@link LucenePage page} in the collection of {@link Page pages}.
	 *
	 * @return the previous {@link LucenePage page} in the collection of {@link Page pages}
	 * or {@literal null} if no {@link LucenePage} proceeds this {@link LucenePage page}.
	 * @see org.springframework.data.gemfire.search.lucene.support.LucenePage
	 * @see #getNext()
	 */
	public LucenePage<T, K, V> getPrevious() {
		return this.previous;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getSize() {
		return getPageSize();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public long getTotalElements() {
		return getQueryResults().size();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public int getTotalPages() {
		long totalElements = getTotalElements();
		int pageSize = getPageSize();
		int totalPages = Double.valueOf(Math.floor(totalElements / pageSize)).intValue();
		totalPages += (totalElements % pageSize != 0 ? 1 : 0);
		return totalPages;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public <S> Page<S> map(Function<? super T, ? extends S> converter) {
		return newListablePage(getContent().stream().map(converter::apply).collect(Collectors.toList()));
	}
}
