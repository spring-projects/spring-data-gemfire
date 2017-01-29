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

package org.springframework.data.gemfire.function.sample;

import org.apache.geode.pdx.PdxInstance;
import org.springframework.data.gemfire.function.ClientCacheFunctionExecutionWithPdxIntegrationTest;
import org.springframework.data.gemfire.function.annotation.OnServer;

/**
 * The ApplicationDomainFunctionExecutions class defines a GemFire Client Cache Function execution targeted at a
 * GemFire Server.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.function.ClientCacheFunctionExecutionWithPdxIntegrationTest
 * @see org.springframework.data.gemfire.function.annotation.OnServer
 * @see org.apache.geode.pdx.PdxInstance
 * @since 1.0.0
 */
@OnServer
@SuppressWarnings("unused")
public interface ApplicationDomainFunctionExecutions {

	Class[] captureConvertedArgumentTypes(String stringValue, Integer integerValue, Boolean booleanValue,
		ClientCacheFunctionExecutionWithPdxIntegrationTest.Person person,
			ClientCacheFunctionExecutionWithPdxIntegrationTest.Gender gender);

	Class[] captureUnconvertedArgumentTypes(String stringValue, Integer integerValue, Boolean booleanValue,
		Object person, Object gender);

	String getAddressField(ClientCacheFunctionExecutionWithPdxIntegrationTest.Address address, String fieldName);

	Object getDataField(PdxInstance data, String fieldName);

}
