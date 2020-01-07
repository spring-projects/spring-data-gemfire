/*
 * Copyright 2016-2020 the original author or authors.
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

package org.springframework.data.gemfire.search.lucene;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptorFactory;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.projection.SpelAwareProxyProjectionFactory;

/**
 * {@link ProjectingLuceneAccessor} is an abstract class supporting implementations of
 * the {@link ProjectingLuceneOperations} interface encapsulating common functionality
 * necessary to execute Lucene queries and work with application domain object views.
 *
 * @author John Blum
 * @see java.lang.ClassLoader
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneOperations
 * @see org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptorFactory
 * @see org.springframework.data.projection.ProjectionFactory
 * @see org.springframework.data.projection.SpelAwareProxyProjectionFactory
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneQuery
 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.LuceneServiceProvider
 * @since 1.1.0
 */
public abstract class ProjectingLuceneAccessor extends LuceneTemplate
		implements BeanClassLoaderAware, BeanFactoryAware, ProjectingLuceneOperations {

	private BeanFactory beanFactory;

	private ClassLoader beanClassLoader;

	private ProjectionFactory projectionFactory;

	/**
	 * Constructs a default, uninitialized instance of the {@link ProjectingLuceneAccessor}.
	 */
	public ProjectingLuceneAccessor() {
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneAccessor} initialized with the given {@link LuceneIndex}
	 * used to perform Lucene queries (searches).
	 *
	 * @param luceneIndex {@link LuceneIndex} used in Lucene queries.
	 * @see org.apache.geode.cache.lucene.LuceneIndex
	 */
	public ProjectingLuceneAccessor(LuceneIndex luceneIndex) {
		super(luceneIndex);
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneAccessor} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param region {@link Region} on which Lucene queries are executed.
	 * @see org.apache.geode.cache.Region
	 */
	public ProjectingLuceneAccessor(String indexName, Region<?, ?> region) {
		super(indexName, region);
	}

	/**
	 * Constructs an instance of the {@link ProjectingLuceneAccessor} initialized with the given Lucene index name
	 * and {@link Region} reference upon which Lucene queries are executed.
	 *
	 * @param indexName {@link String} containing the name of the {@link LuceneIndex} used in Lucene queries.
	 * @param regionPath {@link String} containing the name of the {@link Region} on which Lucene queries are executed.
	 */
	public ProjectingLuceneAccessor(String indexName, String regionPath) {
		super(indexName, regionPath);
	}

	/**
	 * @inheritDoc
	 * @see #resolveProjectionFactory()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		this.projectionFactory = resolveProjectionFactory();
	}

	/**
	 * Null-safe method to resolve the Spring Data {@link ProjectionFactory} used to create projections
	 * out of the Lucene query results.
	 *
	 * @return a resolved instance of the Spring Data {@link ProjectionFactory} used to create projections
	 * out of the Lucene query results.
	 * @see org.springframework.data.projection.ProjectionFactory
	 * @see org.springframework.data.projection.SpelAwareProxyProjectionFactory
	 * @see #afterPropertiesSet()
	 */
	protected ProjectionFactory resolveProjectionFactory() {
		return Optional.ofNullable(getProjectionFactory()).orElseGet(() -> {
			SpelAwareProxyProjectionFactory projectionFactory = new SpelAwareProxyProjectionFactory();
			projectionFactory.setBeanClassLoader(getBeanClassLoader());
			projectionFactory.setBeanFactory(getBeanFactory());
			projectionFactory.registerMethodInvokerFactory(PdxInstanceMethodInterceptorFactory.INSTANCE);
			return setThenGetProjectionFactory(projectionFactory);
		});
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		this.beanClassLoader = beanClassLoader;
	}

	/**
	 * Returns a reference to the {@link ClassLoader} used by the Spring {@link BeanFactory container}
	 * to load bean class definitions.
	 *
	 * @return a reference to the {@link ClassLoader} used by the Spring {@link BeanFactory container}
	 * to load bean class definitions.
	 * @see org.springframework.beans.factory.BeanClassLoaderAware#setBeanClassLoader(ClassLoader)
	 * @see java.lang.ClassLoader
	 */
	protected ClassLoader getBeanClassLoader() {
		return this.beanClassLoader;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory container}.
	 *
	 * @return a reference to the Spring {@link BeanFactory container}.
	 * @see org.springframework.beans.factory.BeanFactoryAware#setBeanFactory(BeanFactory)
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		return this.beanFactory;
	}

	protected ProjectionFactory setThenGetProjectionFactory(ProjectionFactory projectionFactory) {
		setProjectionFactory(projectionFactory);
		return getProjectionFactory();
	}

	/**
	 * Sets the Spring Data {@link ProjectionFactory} used to create projections out of query results.
	 *
	 * @param projectionFactory Spring Data {@link ProjectionFactory} used to created projects out of query results.
	 * @see org.springframework.data.projection.ProjectionFactory
	 */
	public void setProjectionFactory(ProjectionFactory projectionFactory) {
		this.projectionFactory = projectionFactory;
	}

	/**
	 * Returns the Spring Data {@link ProjectionFactory} used to create projections out of query results.
	 *
	 * @return the Spring Data {@link ProjectionFactory} used to created projects out of query results.
	 * @see org.springframework.data.projection.ProjectionFactory
	 */
	protected ProjectionFactory getProjectionFactory() {
		return this.projectionFactory;
	}

	public <T, K, V> List<T> project(List<LuceneResultStruct<K, V>> source, Class<T> projectionType) {
		return source.stream().map(luceneResultStruct -> project(luceneResultStruct, projectionType))
			.collect(Collectors.toList());
	}

	public <T, K, V> T project(LuceneResultStruct<K, V> source, Class<T> projectionType) {
		return project(source.getValue(), projectionType);
	}

	public <T> T project(Object source, Class<T> projectionType) {
		return getProjectionFactory().createProjection(projectionType, source);
	}
}
