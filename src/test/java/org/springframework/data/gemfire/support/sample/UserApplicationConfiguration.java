/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support.sample;

import javax.sql.DataSource;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.data.gemfire.support.SpringContextBootstrappingInitializerIntegrationTest;

/**
 * The UserApplicationConfiguration class is a configuration component for configuring the user's application.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportResource
 * @since 1.4.0
 */
@Configuration
@ImportResource({ "classpath:org/springframework/data/gemfire/support/sample/initializer-gemfire-context.xml" })
@SuppressWarnings("unused")
public class UserApplicationConfiguration {

	@Bean
	public DataSource userDataSource() {
		return new SpringContextBootstrappingInitializerIntegrationTest.TestDataSource();
	}

}
