/*
 * Copyright 2002-2019 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.support.ListRegionsOnServerFunction;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.management.internal.cli.domain.RegionInformation;
import com.gemstone.gemfire.management.internal.cli.functions.GetRegionsFunction;

/**
 * A Spring {@link BeanFactoryPostProcessor} used to register a Client Region Proxy bean for each Region
 * accessible to a GemFire DataSource. If the Region is already defined, the bean definition will not be overridden.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate
 * @see org.springframework.data.gemfire.support.ListRegionsOnServerFunction
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientRegionFactory
 * @see com.gemstone.gemfire.cache.execute.Function
 * @see com.gemstone.gemfire.management.internal.cli.functions.GetRegionsFunction
 * @since 1.2.0
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryPostProcessor {

	protected final Log logger = LogFactory.getLog(getClass());

	private final ClientCache clientCache;

	/**
	 * Constructs an instance of the GemfireDataSourcePostProcessor BeanFactoryPostProcessor class initialized
	 * with the specified GemFire ClientCache instance for creating client PROXY Regions for all data Regions
	 * configured in the GemFire cluster.
	 *
	 * @param clientCache the GemFire ClientCache instance.
	 * @see com.gemstone.gemfire.cache.client.ClientCache
	 */
	public GemfireDataSourcePostProcessor(final ClientCache clientCache) {
		this.clientCache = clientCache;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor#postProcessBeanFactory(org.springframework.beans.factory.config.ConfigurableListableBeanFactory)
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		createClientRegionProxies(beanFactory, regionNames());
	}

	/* (non-Javadoc) */
	Iterable<String> regionNames() {
		try {
			return execute(new ListRegionsOnServerFunction());
		}
		catch (Exception ignore) {
			try {
				Object results = execute(new GetRegionsFunction());
				List<String> regionNames = Collections.emptyList();

				if (containsRegionInformation(results)) {
					Object[] resultsArray = (Object[]) results;

					regionNames = new ArrayList<String>(resultsArray.length);

					for (Object result : resultsArray) {
						regionNames.add(((RegionInformation) result).getName());
					}
				}

				return regionNames;
			}
			catch (Exception e) {
				log("Failed to determine the Regions available on the Server: %n%1$s", e);
				return Collections.emptyList();
			}
		}
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	<T> T execute(Function gemfireFunction, Object... arguments) {
		return new GemfireOnServersFunctionTemplate(clientCache).executeAndExtract(gemfireFunction, arguments);
	}

	/* (non-Javadoc) */
	boolean containsRegionInformation(Object results) {
		return (results instanceof Object[] && ((Object[]) results).length > 0
			&& ((Object[]) results)[0] instanceof RegionInformation);
	}

	/* (non-Javadoc) */
	void createClientRegionProxies(ConfigurableListableBeanFactory beanFactory, Iterable<String> regionNames) {
		if (regionNames.iterator().hasNext()) {
			ClientRegionFactory<?, ?> clientRegionFactory = clientCache.createClientRegionFactory(ClientRegionShortcut.PROXY);

			for (String regionName : regionNames) {
				boolean createRegion = true;

				if (beanFactory.containsBean(regionName)) {
					Object existingBean = beanFactory.getBean(regionName);

					Assert.isTrue(existingBean instanceof Region, String.format(
						"Cannot create a client PROXY Region bean named '%1$s'. A bean with this name of type '%2$s' already exists.",
							regionName, ObjectUtils.nullSafeClassName(existingBean)));

					createRegion = false;
				}

				if (createRegion) {
					log("Creating Region bean with name '%s'...", regionName);
					beanFactory.registerSingleton(regionName, clientRegionFactory.create(regionName));
				}
				else {
					log("A Region with name '%s' is already defined.", regionName);
				}
			}
		}
	}

	/* (non-Javadoc) */
	void log(String message, Object... arguments) {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format(message, arguments));
		}
	}

}
