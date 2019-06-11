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
package org.springframework.data.gemfire.config.annotation.support;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.core.OrderComparator;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

/**
 * Abstract base class for {@link Configurer} interface implementations, encapsulating logic and functionality
 * common to all lazy resolving, composable {@link Configurer} implementations.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the configurable Spring component processed by this {@link Configurer}.
 * @param <C> {@link Class sub-Class} type of {@link Configurer}.
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.core.OrderComparator
 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
 * @since 2.2.0
 */
public abstract class AbstractLazyResolvingComposableConfigurer<T, C extends Configurer<T>>
		implements BeanFactoryAware, Configurer<T> {

	private BeanFactory beanFactory;

	private List<C> configurers = Collections.emptyList();

	/**
	 * Sets a reference to the configured Spring {@link BeanFactory}.
	 *
	 * @param beanFactory reference to the configured Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	@Override
	public void setBeanFactory(@Nullable BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the configured Spring {@link BeanFactory}.
	 *
	 * @return a reference to the configured Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected Optional<BeanFactory> getBeanFactory() {
		return Optional.ofNullable(this.beanFactory);
	}

	/**
	 * Returns the primary {@link Class} type of the {@link Configurer} composed by this {@link Configurer}.
	 *
	 * @return the primary {@link Class} type of the {@link Configurer} composed by this {@link Configurer}.
	 * @see java.lang.Class
	 */
	protected abstract @NonNull Class<C> getConfigurerType();

	/**
	 * Resolves the {@link Configurer Configurers} defined, declared and registered in the Spring application context.
	 *
	 * @return a {@link Stream} of {@link Configurer} objects defined, declared and registered in the Spring
	 * application context.
	 * @see org.springframework.data.gemfire.config.annotation.support.Configurer
	 * @see java.util.stream.Stream
	 */
	protected @NonNull Stream<C> resolveConfigurers() {

		return Optional.ofNullable(this.configurers)
			.filter(it -> !it.isEmpty())
			.orElseGet(() -> getBeanFactory()
				.filter(ListableBeanFactory.class::isInstance)
				.map(ListableBeanFactory.class::cast)
				.map(beanFactory -> {

					Map<String, C> beansOfType =
						beanFactory.getBeansOfType(getConfigurerType(), true, false);

					this.configurers = beansOfType.values().stream()
						.sorted(new OrderComparator())
						.collect(Collectors.toList());

					return this.configurers;

				})
				.orElseGet(Collections::emptyList))
			.stream();
	}

	/**
	 * Applies the configuration from the composition of {@link Configurer Configurers} composed by
	 * this {@link Configurer} to the given Spring component (bean).
	 *
	 * @param beanName {@link String} containing the name of the Spring bean.
	 * @param bean Spring component used to construct, configure and initialize the {@link Object}.
	 * @see #resolveConfigurers()
	 */
	@Override
	public synchronized void configure(String beanName, T bean) {

		resolveConfigurers().forEach(configurer ->
			configurer.configure(beanName, bean));
	}

	/**
	 * Configures the Spring {@link BeanFactory} used to resolve {@link Configurer Configurers} from the Spring context.
	 *
	 * @param <S> sub-class type of {@link Configurer}.
	 * @param beanFactory reference to the Spring {@link BeanFactory}.
	 * @return this {@link AbstractLazyResolvingComposableConfigurer}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #setBeanFactory(BeanFactory)
	 */
	@SuppressWarnings("unchecked")
	public <S extends AbstractLazyResolvingComposableConfigurer<T, C>> S with(@Nullable BeanFactory beanFactory) {

		setBeanFactory(beanFactory);

		return (S) this;
	}
}
