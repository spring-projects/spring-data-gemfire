/*
 * Copyright 2002-2013 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.function.execution.GemfireFunctionOperations;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.support.ListRegionsOnServerFunction;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

/**
 * @author David Turanski
 * a {@link BeanFactoryPostProcessor} to register a Client Region bean, if necessary, for each Region accessible
 * to a Gemfire data source. If the Region is already defined, the definition will not be overridden.
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryPostProcessor {
	private static Log logger = LogFactory.getLog(GemfireDataSourcePostProcessor.class);
	private final ClientCache cache;

	public GemfireDataSourcePostProcessor(ClientCache cache) {
		this.cache = cache;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor#postProcessBeanFactory(org.springframework.beans.factory.config.ConfigurableListableBeanFactory)
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		createClientRegions(beanFactory);

	}

	private void createClientRegions(ConfigurableListableBeanFactory beanFactory) {
		GemfireFunctionOperations template = new GemfireOnServersFunctionTemplate(cache);
		Iterable<String> regionNames = template.executeAndExtract(new ListRegionsOnServerFunction());

		ClientRegionFactory<?, ?> clientRegionFactory = null;
		if (regionNames != null && regionNames.iterator().hasNext()) {
			clientRegionFactory = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);
		}

		for (String regionName : regionNames) {
			boolean createRegion = true;
			if (beanFactory.containsBean(regionName)) {
				Object existingBean = beanFactory.getBean(regionName);
				Assert.isTrue(beanFactory.getBean(regionName) instanceof Region, String.format(
						"cannot create a ClientRegion bean named %s. A bean with this name of type %s already exists.",
						regionName, existingBean.getClass().getName()));
				createRegion = false;
			}
			if (createRegion) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("creating client region for %s", regionName));
				}
				beanFactory.registerSingleton(regionName, clientRegionFactory.create(regionName));
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("a region named %s is already defined",regionName));
				}
			}
		}
	}
}
