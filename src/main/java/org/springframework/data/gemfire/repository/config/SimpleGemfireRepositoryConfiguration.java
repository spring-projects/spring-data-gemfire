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
package org.springframework.data.gemfire.repository.config;

import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.config.SimpleGemfireRepositoryConfiguration.GemfireRepositoryConfiguration;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AutomaticRepositoryConfigInformation;
import org.springframework.data.repository.config.ManualRepositoryConfigInformation;
import org.springframework.data.repository.config.RepositoryConfig;
import org.springframework.data.repository.config.SingleRepositoryConfigInformation;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

/**
 * Repository configuration implementation.
 * 
 * @author Oliver Gierke
 */
class SimpleGemfireRepositoryConfiguration extends
		RepositoryConfig<GemfireRepositoryConfiguration, SimpleGemfireRepositoryConfiguration> {

	private static final String GEMFIRE_TEMPLATE_REF = "gemfire-template-ref";

	/**
	 * Creates a new {@link SimpleGemfireRepositoryConfiguration} for the given {@link Element}.
	 * 
	 * @param repositoriesElement must not be {@literal null}.
	 */
	protected SimpleGemfireRepositoryConfiguration(Element repositoriesElement) {
		super(repositoriesElement, GemfireRepositoryFactoryBean.class.getName());
	}

	/**
	 * Returns the bean name of the {@link GemfireTemplate} to be used.
	 * 
	 * @return
	 */
	String getGemfireTemplateRef() {

		String attribute = getSource().getAttribute(GEMFIRE_TEMPLATE_REF);
		return StringUtils.hasText(attribute) ? attribute : null;
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.GlobalRepositoryConfigInformation#getAutoconfigRepositoryInformation(java.lang.String)
	 */
	@Override
	public GemfireRepositoryConfiguration getAutoconfigRepositoryInformation(String interfaceName) {
		return new AutomaticGemfireRepositoryConfiguration(interfaceName, this);
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.CommonRepositoryConfigInformation#getNamedQueriesLocation()
	 */
	@Override
	public String getNamedQueriesLocation() {
		return "classpath*:META-INF/gemfire-named-queries.properties";
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfig#createSingleRepositoryConfigInformationFor(org.w3c.dom.Element)
	 */
	@Override
	protected GemfireRepositoryConfiguration createSingleRepositoryConfigInformationFor(Element element) {
		return new ManualGemfireRepositoryConfiguration(element, this);
	}

	public interface GemfireRepositoryConfiguration extends
			SingleRepositoryConfigInformation<SimpleGemfireRepositoryConfiguration> {

		String getGemfireTemplateRef();
	}

	static class ManualGemfireRepositoryConfiguration extends
			ManualRepositoryConfigInformation<SimpleGemfireRepositoryConfiguration> implements GemfireRepositoryConfiguration {

		/**
		 * @param element
		 * @param parent
		 */
		public ManualGemfireRepositoryConfiguration(Element element, SimpleGemfireRepositoryConfiguration parent) {
			super(element, parent);
		}

		/* 
		 * (non-Javadoc)
		 * @see org.springframework.data.gemfire.config.GemfireRepositoryParser.SimpleGemfireRepositoryConfiguration.GemfireRepositoryConfiguration#getGemfireTemplateRef()
		 */
		@Override
		public String getGemfireTemplateRef() {
			return getAttribute(GEMFIRE_TEMPLATE_REF);
		}

	}

	static class AutomaticGemfireRepositoryConfiguration extends
			AutomaticRepositoryConfigInformation<SimpleGemfireRepositoryConfiguration> implements
			GemfireRepositoryConfiguration {

		/**
		 * @param interfaceName
		 * @param parent
		 */
		public AutomaticGemfireRepositoryConfiguration(String interfaceName, SimpleGemfireRepositoryConfiguration parent) {
			super(interfaceName, parent);
		}

		/* 
		 * (non-Javadoc)
		 * @see org.springframework.data.gemfire.config.GemfireRepositoryParser.SimpleGemfireRepositoryConfiguration.GemfireRepositoryConfiguration#getGemfireTemplateRef()
		 */
		@Override
		public String getGemfireTemplateRef() {

			return getParent().getGemfireTemplateRef();
		}
	}
}