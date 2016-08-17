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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;
import java.util.Properties;

import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.Assert;

/**
 * The SslConfiguration class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class SslConfiguration extends EmbeddedServiceConfigurationSupport {

	@Override
	protected Class getAnnotationType() {
		return EnableSsl.class;
	}

	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = new PropertiesBuilder();

		EnableSsl.Component[] components = (EnableSsl.Component[]) annotationAttributes.get("components");

		Assert.notNull(components, "GemFire SSL enabled components cannot be null");

		for (EnableSsl.Component component : components) {
			gemfireProperties
				.setProperty(String.format("%s-ssl-ciphers", component), annotationAttributes.get("ciphers"))
				.setProperty(String.format("%s-ssl-enabled", component), Boolean.TRUE.toString())
				.setProperty(String.format("%s-ssl-keystore", component), annotationAttributes.get("keystore"))
				.setProperty(String.format("%s-ssl-keystore-password", component), annotationAttributes.get("keystorePassword"))
				.setProperty(String.format("%s-ssl-keystore-type", component), annotationAttributes.get("keystoreType"))
				.setProperty(String.format("%s-ssl-protocols", component), annotationAttributes.get("protocols"))
				.setProperty(String.format("%s-ssl-require-authentication", component),
					Boolean.TRUE.equals(annotationAttributes.get("requireAuthentication")))
				.setProperty(String.format("%s-ssl-truststore", component), annotationAttributes.get("truststore"))
				.setProperty(String.format("%s-ssl-truststore-password", component), annotationAttributes.get("truststorePassword"));
		}

		return gemfireProperties.build();
	}
}
