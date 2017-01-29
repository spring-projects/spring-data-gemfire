/*
 * Copyright 2012-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.repository.cdi;

import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.enterprise.event.Observes;
import javax.enterprise.inject.spi.AfterBeanDiscovery;
import javax.enterprise.inject.spi.Bean;
import javax.enterprise.inject.spi.BeanManager;
import javax.enterprise.inject.spi.ProcessBean;

import org.apache.geode.cache.Region;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.repository.cdi.CdiRepositoryBean;
import org.springframework.data.repository.cdi.CdiRepositoryExtensionSupport;

/**
 * The GemfireRepositoryExtension class...
 *
 * @author John Blum
 * @see org.springframework.data.repository.cdi.CdiRepositoryExtensionSupport
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public class GemfireRepositoryExtension extends CdiRepositoryExtensionSupport {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	final Map<Set<Annotation>, Bean<GemfireMappingContext>> mappingContexts =
		new HashMap<Set<Annotation>, Bean<GemfireMappingContext>>();

	final Set<Bean<Region>> regionBeans = new HashSet<Bean<Region>>();

	/* (non-Javadoc) */
	public GemfireRepositoryExtension() {
		logger.info("Activating CDI extension for Spring Data GemFire Repositories");
	}

	/**
	 * Implementation of an observer that captures GemFire Region beans defined in the CDI container, storing them
	 * along with any defined GemfireMappingContexts for later construction of the Repository beans.
	 *
	 * @param <X> class type of the bean instance.
	 * @param processBean annotated type as defined by CDI.
	 * @see javax.enterprise.inject.spi.ProcessBean
	 * @see javax.enterprise.event.Observes
	 */
	@SuppressWarnings("unchecked")
	<X> void processBean(@Observes ProcessBean<X> processBean) {
		Bean<X> bean = processBean.getBean();

		for (Type type : bean.getTypes()) {
			Type resolvedType = (type instanceof ParameterizedType ? ((ParameterizedType) type).getRawType() : type);

			if (resolvedType instanceof Class<?>) {
				Class<?> classType = (Class<?>) resolvedType;

				if (Region.class.isAssignableFrom(classType)) {
					logger.debug("Found Region bean with name {}", bean.getName());
					regionBeans.add((Bean<Region>) bean);
				}
				else if (GemfireMappingContext.class.isAssignableFrom(classType)) {
					logger.debug("Discovered {} bean with types {} having qualifiers {}",
						GemfireMappingContext.class.getName(), bean.getTypes(), bean.getQualifiers());
					mappingContexts.put(bean.getQualifiers(), (Bean<GemfireMappingContext>) bean);
				}
			}
		}
	}

	/**
	 * Implementation of an observer that registers beans in the CDI container for the detected Spring Data
	 * Repositories.
	 *
	 * Repository beans are associated to the appropriate GemfireMappingContexts based on their qualifiers.
	 *
	 * @param beanManager the BeanManager instance.
	 * @see javax.enterprise.inject.spi.AfterBeanDiscovery
	 * @see javax.enterprise.inject.spi.BeanManager
	 * @see javax.enterprise.event.Observes
	 */
	void afterBeanDiscovery(@Observes AfterBeanDiscovery afterBeanDiscovery, BeanManager beanManager) {
		for (Map.Entry<Class<?>, Set<Annotation>> entry : getRepositoryTypes()) {
			Class<?> repositoryType = entry.getKey();
			Set<Annotation> qualifiers = entry.getValue();

			// Create the bean representing the Repository.
			CdiRepositoryBean<?> repositoryBean = createRepositoryBean(beanManager, repositoryType, qualifiers);

			logger.info("Registering bean for '{}' with qualifiers {}.", repositoryType.getName(), qualifiers);

			// Register the bean with the extension and the container.
			registerBean(repositoryBean);
			afterBeanDiscovery.addBean(repositoryBean);
		}
	}

	/* (non-Javadoc) */
	<T> CdiRepositoryBean<T> createRepositoryBean(BeanManager beanManager, Class<T> repositoryType,
			Set<Annotation> qualifiers) {

		// Determine the GemfireMappingContext bean that matches the qualifiers of the Repository.
		Bean<GemfireMappingContext> gemfireMappingContextBean = mappingContexts.get(qualifiers);

		// Construct and return a GemFire Repository bean.
		return new GemfireRepositoryBean<T>(beanManager, repositoryType, qualifiers, getCustomImplementationDetector(),
			gemfireMappingContextBean, regionBeans);
	}

}
