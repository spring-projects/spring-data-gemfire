/*
 * Copyright 2018 the original author or authors.
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

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer;
import org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean;
import org.springframework.lang.Nullable;

/**
 * Composition for {@link IndexConfigurer}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.IndexConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractLazyResolvingComposableConfigurer
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @since 2.2.0
 */
public class LazyResolvingComposableIndexConfigurer
		extends AbstractLazyResolvingComposableConfigurer<IndexFactoryBean, IndexConfigurer>
		implements IndexConfigurer {

	public static LazyResolvingComposableIndexConfigurer create() {
		return create(null);
	}

	public static LazyResolvingComposableIndexConfigurer create(@Nullable BeanFactory beanFactory) {
		return new LazyResolvingComposableIndexConfigurer().with(beanFactory);
	}

	@Override
	protected Class<IndexConfigurer> getConfigurerType() {
		return IndexConfigurer.class;
	}

	@Override
	public void configure(String beanName, LuceneIndexFactoryBean luceneIndexFactoryBean) {

		resolveConfigurers().forEach(configurer ->
			configurer.configure(beanName, luceneIndexFactoryBean));
	}
}
