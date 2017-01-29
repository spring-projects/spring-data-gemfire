/*
 * Copyright 2002-2018 the original author or authors.
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
package org.springframework.data.gemfire.function.execution;

import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.FunctionService;
import org.springframework.util.Assert;

/**
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionExecution
 * @see org.apache.geode.cache.execute.Execution
 * @see org.apache.geode.cache.execute.FunctionService
 */
class GroupMemberFunctionExecution extends AbstractFunctionExecution {

	private final String[] groups;

	/**
	 * Constructs an instance of the GroupMemberFunctionExecution class to execute a data independent Function
	 * on a single member from each of the specified groups.
	 *
	 * @param groups the list of GemFire Groups from which to pick a member from each group on which to execute
	 * the data independent Function.
	 */
	public GroupMemberFunctionExecution(final String... groups) {
		Assert.notEmpty(groups, "'groups' cannot be null or empty.");
		this.groups = groups;
	}

	/**
	 * Executes the data independent Function on a single member from each of the specified groups.
	 *
	 * @return an Execution to execute the Function.
	 * @see org.apache.geode.cache.execute.FunctionService#onMember(String...)
	 */
	@Override
	protected Execution getExecution() {
		return FunctionService.onMember(this.groups);
	}

}
