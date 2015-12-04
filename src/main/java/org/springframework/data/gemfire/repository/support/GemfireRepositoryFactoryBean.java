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

package org.springframework.data.gemfire.repository.support;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;

/**
 * {@link FactoryBean} adapter for {@link GemfireRepositoryFactory}.
 * 
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
 * @see org.springframework.data.mapping.context.MappingContext
 * @see org.springframework.data.repository.Repository
 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport
 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
 * @see com.gemstone.gemfire.cache.Region
 */
@SuppressWarnings("unused")
public class GemfireRepositoryFactoryBean<T extends Repository<S, ID>, S, ID extends Serializable>
		extends RepositoryFactoryBeanSupport<T, S, ID> implements ApplicationContextAware {

	protected static final String DEFAULT_MAPPING_CONTEXT_BEAN_NAME =
		String.format("%1$s.%2$s", GemfireMappingContext.class.getName(), "DEFAULT");

	private ApplicationContext applicationContext;

	private Iterable<Region<?, ?>> regions;

	private MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> context;

	/**
	 * Sets a reference to the Spring {@link ApplicationContext} in which this object runs.
	 *
	 * @param applicationContext the Spring {@link ApplicationContext} reference.
	 * @see org.springframework.context.ApplicationContextAware#setApplicationContext(ApplicationContext)
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		Collection<Region> regions = applicationContext.getBeansOfType(Region.class).values();
		this.regions = (Iterable) Collections.unmodifiableCollection(regions);
		this.applicationContext = applicationContext;
	}

	/**
	 * Gets a reference to the Spring {@link ApplicationContext} in which this object runs.
	 *
	 * @return a reference to the Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContext
	 * @see #setApplicationContext(ApplicationContext)
	 */
	protected ApplicationContext getApplicationContext() {
		Assert.state(applicationContext != null, "The Spring ApplicationContext was not properly initialized");
		return applicationContext;
	}

	/**
	 * Configures the {@link MappingContext} used to perform domain object type to store mappings.
	 * 
	 * @param mappingContext the {@link MappingContext} to set.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 */
	public void setGemfireMappingContext(MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {
		setMappingContext(mappingContext);
		this.context = mappingContext;
	}

	/**
	 * Gets a reference to the {@link MappingContext} used to perform domain object type to store mappings.
	 *
	 * @return a reference to the {@link MappingContext}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see org.springframework.data.mapping.context.MappingContext
	 * @see #setGemfireMappingContext(MappingContext)
	 */
	protected MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> getGemfireMappingContext() {
		return this.context;
	}

	/* (non-Javadoce) */
	Iterable<Region<?, ?>> getRegions() {
		return regions;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() {
		resolveMappingContext();
		super.afterPropertiesSet();
	}

	/**
	 * Creates an instance of {@link RepositoryFactorySupport} that interfaces with GemFire.
	 *
	 * @see org.springframework.data.repository.core.support.RepositoryFactoryBeanSupport#createRepositoryFactory()
	 * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
	 */
	@Override
	protected RepositoryFactorySupport createRepositoryFactory() {
		return new GemfireRepositoryFactory(regions, context);
	}

	/**
	 * Resolves the appropriate Spring Data {@link MappingContext} to use for handling entity domain object type
	 * to store mappings.
	 *
	 * @see org.springframework.data.mapping.context.MappingContext
	 * @see #getApplicationContext()
	 * @see #setMappingContext(MappingContext)
	 */
	protected void resolveMappingContext() {
		if (getGemfireMappingContext() == null) {
			ApplicationContext applicationContext = getApplicationContext();

			GemfireMappingContext gemfireMappingContext;

			try {
				gemfireMappingContext = applicationContext.getBean(DEFAULT_MAPPING_CONTEXT_BEAN_NAME,
					GemfireMappingContext.class);
			}
			catch (BeansException ignore) {
				gemfireMappingContext = new GemfireMappingContext();

				if (applicationContext instanceof ConfigurableApplicationContext) {
					((ConfigurableApplicationContext) applicationContext).getBeanFactory()
						.registerSingleton(DEFAULT_MAPPING_CONTEXT_BEAN_NAME, gemfireMappingContext);
				}
			}

			setGemfireMappingContext(gemfireMappingContext);
		}
	}

}
