/*
 * Copyright 2017-2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.query.Index;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.Configurer;
import org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean;

/**
 * The {@link IndexConfigurer} interface defines a contract for implementing {@link Object Objects} in order to
 * customize the configuration of Entity-defined {@link Index Indexes} when a user annotates her Spring application
 * {@link Configuration} {@link Class} with {@link EnableIndexing}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableIndexing
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @since 2.0.0
 */
public interface IndexConfigurer extends Configurer<IndexFactoryBean> {

	/**
	 * Configuration callback method providing a reference to a {@link IndexFactoryBean} used to construct, configure
	 * and initialize an instance of a peer {@link Index}.
	 *
	 * @param beanName name of the {@link Index} bean declared in the Spring application context.
	 * @param bean reference to the {@link IndexFactoryBean}.
	 * @see org.springframework.data.gemfire.IndexFactoryBean
	 */
	default void configure(String beanName, IndexFactoryBean bean) { }

	/**
	 * Configuration callback method providing a reference to a {@link LuceneIndexFactoryBean} used to construct,
	 * configure and initialize an instance of a peer {@link LuceneIndex}.
	 *
	 * @param beanName name of the {@link LuceneIndex} bean declared in the Spring application context.
	 * @param bean reference to the {@link LuceneIndexFactoryBean}.
	 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
	 */
	default void configure(String beanName, LuceneIndexFactoryBean bean) { }

}
